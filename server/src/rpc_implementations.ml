open! Core
open! Async
open Tic_tac_toe_2023_common
open Protocol

let info = Log.Global.info "%s"

let me ~global_state:_ =
  Rpc.Rpc.implement' Me.rpc (fun (user_state : User_state.t) () ->
    Username.of_string user_state.user)
;;

let create_game ~global_state =
  Rpc.Rpc.implement'
    Create_game.rpc
    (fun (user_state : User_state.t) { txt = game_proposal; username } ->
    let { Create_game.Query.game_kind; against_server_bot } =
      game_proposal
    in
    let against =
      match against_server_bot with
      | None -> "human player"
      | Some difficulty ->
        [%string "server bot on %{difficulty#Difficulty} difficulty"]
    in
    info
      [%string
        "%{user_state.user} as (%{username#Username}) proposed \
         '%{game_kind#Game_kind} against %{against}'"];
    State.create_game
      global_state
      ~first_player:username
      ~query:game_proposal)
;;

let join_existing_game ~global_state =
  Rpc.Rpc.implement'
    Join_existing_game.rpc
    (fun (user_state : User_state.t) { txt = game_id; username } ->
    info
      [%string
        "%{user_state.user} as (%{username#Username}) wants to join \
         '%{game_id#Game_id}'"];
    State.join_game global_state ~second_player:username ~game_id)
;;

let show_all_games_with_two_players ~global_state =
  Rpc.Rpc.implement'
    Show_all_games_with_two_players.rpc
    (fun (user_state : User_state.t) () ->
    info
      [%string "%{user_state.user} wants to view all games with two players"];
    State.get_all_running_games global_state)
;;

let list_all_joinable_games ~global_state =
  Rpc.Rpc.implement'
    List_all_joinable_games.rpc
    (fun (user_state : User_state.t) () ->
    info [%string "%{user_state.user} wants to view all joinable games"];
    State.get_all_joinable_games global_state)
;;

let get_game ~global_state =
  Rpc.Rpc.implement' Get_game.rpc (fun (user_state : User_state.t) game_id ->
    info [%string "%{user_state.user} wants to view %{game_id#Game_id}"];
    State.get_game global_state ~game_id)
;;

let take_turn ~global_state =
  Rpc.Rpc.implement
    Take_turn.rpc
    (fun
      (user_state : User_state.t)
      { txt = { game_id; position }; username }
    ->
    info
      [%string
        "%{user_state.user} as (%{username#Username}) wants to play \
         '%{position#Position}' in game %{game_id#Game_id}"];
    State.take_turn global_state ~game_id ~position ~username)
;;

let is_thinking ~global_state =
  Rpc.Rpc.implement'
    Is_thinking.rpc
    (fun (user_state : User_state.t) game_id ->
    info
      [%string
        "%{user_state.user}  wants to know if game id '%{game_id#Game_id}' \
         is thinking."];
    State.is_thinking global_state ~game_id)
;;

let implementations ~global_state =
  Rpc.Implementations.create_exn
    ~implementations:
      [ me ~global_state
      ; create_game ~global_state
      ; join_existing_game ~global_state
      ; show_all_games_with_two_players ~global_state
      ; list_all_joinable_games ~global_state
      ; get_game ~global_state
      ; take_turn ~global_state
      ; is_thinking ~global_state
      ]
    ~on_unknown_rpc:`Continue
;;
