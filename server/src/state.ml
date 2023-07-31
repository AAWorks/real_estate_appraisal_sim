open! Core
open! Async
open Tic_tac_toe_2023_common
open Protocol

type t =
  { mutable next_id : int
  ; mutable world_state : World_state.t
  ; mutable thinking : Game_id.Set.t
  }

let to_string_hum t =
  let { World_state.running_games; joinable_games } = t.world_state in
  let joinable_games =
    Sexp.to_string
      [%message (joinable_games : Joinable_game.t Game_id.Map.t)]
  in
  let running_games =
    String.concat_lines
      (Map.data running_games |> List.map ~f:Game_state.to_string_hum)
  in
  [%string "next_id: %{t.next_id#Int}\n%{joinable_games}\n%{running_games}"]
;;

let new_id t =
  t.next_id <- succ t.next_id;
  Game_id.of_string (Int.to_string t.next_id)
;;

let create ~world_state =
  { world_state; next_id = 0; thinking = Game_id.Set.empty }
;;

let create_new_joinable_game t ~game_kind ~first_player =
  let game_id = new_id t in
  let new_joinable_game =
    { Joinable_game.game_id
    ; game_kind
    ; player_x = Player.Player first_player
    }
  in
  let new_world_state =
    { t.world_state with
      joinable_games =
        Map.add_exn
          t.world_state.joinable_games
          ~key:game_id
          ~data:new_joinable_game
    }
  in
  t.world_state <- new_world_state;
  game_id
;;

let create_new_game_against_bot t ~game_kind ~first_player ~difficulty =
  let game_id = new_id t in
  let new_game =
    { Game_state.game_id
    ; game_kind
    ; game_status = Game_status.Turn_of X
    ; player_x = Player.Player first_player
    ; player_o = Player.Server_bot difficulty
    ; pieces = Position.Map.empty
    }
  in
  let new_world_state =
    { t.world_state with
      running_games =
        Map.add_exn t.world_state.running_games ~key:game_id ~data:new_game
    }
  in
  t.world_state <- new_world_state;
  game_id
;;

let create_game
  t
  ~first_player
  ~query:{ Create_game.Query.game_kind; against_server_bot }
  =
  match against_server_bot with
  | None -> create_new_joinable_game t ~game_kind ~first_player
  | Some difficulty ->
    create_new_game_against_bot t ~game_kind ~first_player ~difficulty
;;

let start_joinable_game t ~second_player ~joinable_game =
  let new_joinable_games =
    Map.remove
      t.world_state.joinable_games
      joinable_game.Joinable_game.game_id
  in
  let new_running_games =
    let new_game =
      { Game_state.game_id = joinable_game.Joinable_game.game_id
      ; game_kind = joinable_game.game_kind
      ; game_status = Turn_of X
      ; player_x = joinable_game.player_x
      ; player_o = Player.Player second_player
      ; pieces = Position.Map.empty
      }
    in
    Map.add_exn
      t.world_state.running_games
      ~key:joinable_game.game_id
      ~data:new_game
  in
  t.world_state
    <- { joinable_games = new_joinable_games
       ; running_games = new_running_games
       };
  Join_existing_game.Response.Ok
;;

let join_game t ~second_player ~game_id =
  match Map.find t.world_state.joinable_games game_id with
  | Some joinable_game -> start_joinable_game t ~second_player ~joinable_game
  | None ->
    (match Map.find t.world_state.running_games game_id with
     | None -> Game_does_not_exist
     | Some running_game ->
       (match
          List.exists
            [ running_game.player_o; running_game.player_x ]
            ~f:(fun player -> Player.equal player (Player second_player))
        with
        | true -> You've_already_joined_this_game
        | false ->
          (match running_game.game_status with
           | Game_status.Game_over _ -> Game_already_ended
           | Game_status.Turn_of _ -> Game_already_full)))
;;

let get_all_running_games t = t.world_state.running_games
let get_all_joinable_games t = t.world_state.joinable_games

let get_game t ~game_id =
  match Map.find t.world_state.joinable_games game_id with
  | Some x -> Get_game.Response.Waiting_for_someone_to_join x
  | None ->
    (match Map.find t.world_state.running_games game_id with
     | Some x -> Get_game.Response.Both_players x
     | None -> Get_game.Response.Game_id_does_not_exist)
;;

let is_board_full ~game_kind pieces =
  let all_positions_in_bound_invariant =
    Map.for_alli pieces ~f:(fun ~key:position ~data:_ ->
      Position.in_bounds position ~game_kind)
  in
  if not all_positions_in_bound_invariant
  then
    print_s
      [%message
        "soft invariant failed, out of bound positions"
          (game_kind : Game_kind.t)
          (pieces : Piece.t Position.Map.t)];
  let is_board_full =
    let board_length = Game_kind.board_length game_kind in
    Map.length pieces = board_length * board_length
  in
  all_positions_in_bound_invariant && is_board_full
;;

let compute_next_game_status pieces ~current_piece ~game_kind =
  match Traverse.did_piece_win ~piece:current_piece ~game_kind pieces with
  | Some positions ->
    Game_status.Game_over { winner = Some (current_piece, positions) }
  | None ->
    (match is_board_full pieces ~game_kind with
     | true -> Game_status.Game_over { winner = None }
     | false -> Game_status.Turn_of (Piece.flip current_piece))
;;

let rec maybe_take_server_ai_turn t ~game_state : unit Deferred.t =
  match game_state.Game_state.game_status with
  | Game_status.Game_over _ -> Deferred.return ()
  | Game_status.Turn_of piece ->
    (match Game_state.get_player game_state ~piece with
     | Player _ -> Deferred.return ()
     | Server_bot difficulty ->
       t.thinking <- Set.add t.thinking game_state.game_id;
       let%bind.Deferred position =
         In_thread.run (fun () ->
           match difficulty with
           | Easy -> Easy.compute_next_move ~me:piece ~game_state
           | Medium ->
             Medium.compute_next_move ~depth:2 ~me:piece ~game_state
           | Hard ->
             Medium.compute_next_move
               ~depth:
                 (match game_state.game_kind with
                  | Tic_tac_toe -> 10
                  | Omok -> 3)
               ~me:piece
               ~game_state)
       in
       t.thinking <- Set.remove t.thinking game_state.game_id;
       actually_take_turn
         t
         ~game_id:game_state.game_id
         ~position
         ~game_state
         ~piece)

and actually_take_turn
  t
  ~game_id
  ~position
  ~(game_state : Game_state.t)
  ~piece
  =
  let new_running_game_after_next_move =
    let pieces =
      Map.add_exn game_state.Game_state.pieces ~key:position ~data:piece
    in
    let game_status =
      compute_next_game_status
        ~current_piece:piece
        ~game_kind:game_state.game_kind
        pieces
    in
    { Game_state.pieces
    ; game_status
    ; player_o = game_state.player_o
    ; player_x = game_state.player_x
    ; game_id = game_state.game_id
    ; game_kind = game_state.game_kind
    }
  in
  let new_world_state =
    { t.world_state with
      running_games =
        Map.set
          t.world_state.running_games
          ~key:game_id
          ~data:new_running_game_after_next_move
    }
  in
  t.world_state <- new_world_state;
  maybe_take_server_ai_turn t ~game_state:new_running_game_after_next_move
;;

let take_turn t ~game_id ~position ~username
  : Take_turn.Response.t Deferred.t
  =
  let open Deferred.Let_syntax in
  let open Take_turn.Response in
  match Map.find t.world_state.running_games game_id with
  | None -> return Take_turn.Response.Game_does_not_exist
  | Some game_state ->
    (match game_state.game_status with
     | Game_status.Game_over _ -> return Game_is_over
     | Game_status.Turn_of piece ->
       let is_current_player's_turn =
         Player.equal
           (Game_state.get_player game_state ~piece)
           (Player username)
       in
       (match is_current_player's_turn with
        | false ->
          let is_player_one_of_the_players_from_the_game =
            List.exists
              [ game_state.player_x; game_state.player_o ]
              ~f:(Player.equal (Player username))
          in
          (match is_player_one_of_the_players_from_the_game with
           | true -> return Not_your_turn
           | false -> return You_are_not_a_player_in_this_game)
        | true ->
          (match
             Position.in_bounds position ~game_kind:game_state.game_kind
           with
           | false -> return Position_out_of_bounds
           | true ->
             (match Map.find game_state.pieces position with
              | Some _ -> return Position_already_occupied
              | None ->
                let%bind () =
                  actually_take_turn t ~game_id ~position ~game_state ~piece
                in
                return Ok))))
;;

let is_thinking t ~game_id = Set.mem t.thinking game_id
