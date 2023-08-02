open! Core
open Tic_tac_toe_2023_common
open Protocol

let score
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : float
  =
  let my_score = Traverse.score_for_piece ~piece:me ~game_kind pieces in
  let their_score =
    Traverse.score_for_piece ~piece:(Piece.flip me) ~game_kind pieces
  in
  if Float.is_inf their_score then -.their_score else my_score -. their_score
;;

let empty_pieces ~game_kind ~pieces =
  let length = Game_kind.board_length game_kind in
  let all_positions =
    let%bind.List row = List.init length ~f:Fn.id in
    let%map.List column = List.init length ~f:Fn.id in
    { Position.row; column }
  in
  List.filter all_positions ~f:(fun position ->
    not (Map.mem pieces position))
;;

let adjacent_pieces_or_default ~game_kind ~pieces ~default =
  match Map.is_empty pieces with
  | true -> [ default ]
  | false ->
    Map.fold
      pieces
      ~init:Position.Set.empty
      ~f:(fun ~key:position ~data:_ acc ->
      List.fold ~init:acc Position.all_offsets ~f:(fun acc offset ->
        let candidate_position = offset position in
        match
          Position.in_bounds ~game_kind candidate_position
          && not (Map.mem pieces candidate_position)
        with
        | true -> Set.add acc candidate_position
        | false -> acc))
    |> Set.to_list
;;

let next_possible_moves
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  match game_kind with
  | Tic_tac_toe -> empty_pieces ~game_kind ~pieces
  | Omok ->
    adjacent_pieces_or_default
      ~default:{ Position.row = 0; column = 0 }
      ~game_kind
      ~pieces
;;

let%test_module "Next possible moves" =
  (module struct
    let%expect_test "tictactoe" =
      let pieces =
        Position.Map.of_alist_exn
          [ { Position.row = 0; column = 0 }, Piece.X ]
      in
      let result = next_possible_moves ~game_kind:Tic_tac_toe ~pieces in
      print_s [%message (result : Position.t list)];
      [%expect
        {|
    (result
     (((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 0))
      ((row 1) (column 1)) ((row 1) (column 2)) ((row 2) (column 0))
      ((row 2) (column 1)) ((row 2) (column 2)))) |}]
    ;;

    let%expect_test "tictactoe - empty" =
      let pieces = Position.Map.empty in
      let result = next_possible_moves ~game_kind:Tic_tac_toe ~pieces in
      print_s [%message (result : Position.t list)];
      [%expect
        {|
    (result
     (((row 0) (column 0)) ((row 0) (column 1)) ((row 0) (column 2))
      ((row 1) (column 0)) ((row 1) (column 1)) ((row 1) (column 2))
      ((row 2) (column 0)) ((row 2) (column 1)) ((row 2) (column 2)))) |}]
    ;;

    let%expect_test "omok" =
      let pieces =
        Position.Map.of_alist_exn
          [ { Position.row = 0; column = 0 }, Piece.X ]
      in
      let result = next_possible_moves ~game_kind:Omok ~pieces in
      print_s [%message (result : Position.t list)];
      [%expect
        {|
    (result (((row 0) (column 1)) ((row 1) (column 0)) ((row 1) (column 1)))) |}];
      let pieces =
        Position.Map.of_alist_exn
          [ { Position.row = 3; column = 3 }, Piece.X
          ; { Position.row = 3; column = 2 }, Piece.X
          ]
      in
      let result = next_possible_moves ~game_kind:Omok ~pieces in
      print_s [%message (result : Position.t list)];
      [%expect
        {|
     (result
      (((row 2) (column 1)) ((row 2) (column 2)) ((row 2) (column 3))
       ((row 2) (column 4)) ((row 3) (column 1)) ((row 3) (column 4))
       ((row 4) (column 1)) ((row 4) (column 2)) ((row 4) (column 3))
       ((row 4) (column 4)))) |}]
    ;;

    let%expect_test "omok - empty" =
      let pieces = Position.Map.empty in
      let result = next_possible_moves ~game_kind:Omok ~pieces in
      print_s [%message (result : Position.t list)];
      [%expect {|
    (result (((row 0) (column 0)))) |}];
      let pieces =
        Position.Map.of_alist_exn
          [ { Position.row = 3; column = 3 }, Piece.X
          ; { Position.row = 3; column = 2 }, Piece.X
          ]
      in
      let result = next_possible_moves ~game_kind:Omok ~pieces in
      print_s [%message (result : Position.t list)];
      [%expect
        {|
     (result
      (((row 2) (column 1)) ((row 2) (column 2)) ((row 2) (column 3))
       ((row 2) (column 4)) ((row 3) (column 1)) ((row 3) (column 4))
       ((row 4) (column 1)) ((row 4) (column 2)) ((row 4) (column 3))
       ((row 4) (column 4)))) |}]
    ;;
  end)
;;

let rec mimimax
  ~(me : Piece.t)
  ~(am_maximizing_player : bool)
  ~(depth : int)
  ~(pieces : Piece.t Position.Map.t)
  ~(game_kind : Game_kind.t)
  ~(alpha : float)
  ~(beta : float)
  : float
  =
  let node_score = score ~me ~game_kind ~pieces in
  match depth with
  | 0 -> node_score
  | _ ->
    (match Float.is_inf node_score with
     | true -> node_score
     | false ->
       let possible_moves = next_possible_moves ~game_kind ~pieces in
       (match possible_moves with
        | [] -> node_score
        | _ :: _ ->
          let acc_function, init =
            match am_maximizing_player with
            | true -> Float.max, (-1.0 *. Float.infinity, alpha, beta)
            | false -> Float.min, (Float.infinity, alpha, beta)
          in
          let update_alpha_beta
            : value:float -> alpha:float -> beta:float -> float * float
            =
            match am_maximizing_player with
            | true -> fun ~value ~alpha ~beta -> Float.max alpha value, beta
            | false -> fun ~value ~alpha ~beta -> alpha, Float.min beta value
          in
          let should_cutoff
            : value:float -> alpha:float -> beta:float -> bool
            =
            match am_maximizing_player with
            | true -> fun ~value ~alpha:_ ~beta -> Float.O.(value > beta)
            | false -> fun ~value ~alpha ~beta:_ -> Float.O.(value < alpha)
          in
          let out =
            List.fold_until
              possible_moves
              ~init
              ~finish:Tuple3.get1
              ~f:(fun (acc_score, alpha, beta) possible_move ->
              let next_pieces =
                Map.set
                  pieces
                  ~key:possible_move
                  ~data:
                    (match am_maximizing_player with
                     | true -> me
                     | false -> Piece.flip me)
              in
              let score =
                acc_function
                  acc_score
                  (mimimax
                     ~me
                     ~am_maximizing_player:(not am_maximizing_player)
                     ~depth:(depth - 1)
                     ~pieces:next_pieces
                     ~game_kind
                     ~alpha
                     ~beta)
              in
              match should_cutoff ~value:score ~alpha ~beta with
              | true -> Continue_or_stop.Stop score
              | false ->
                let alpha, beta =
                  update_alpha_beta ~value:score ~alpha ~beta
                in
                Continue_or_stop.Continue (score, alpha, beta))
          in
          out))
;;

let compute_next_move ~depth ~me ~(game_state : Game_state.t) : Position.t =
  let next_possible_moves =
    next_possible_moves
      ~game_kind:game_state.game_kind
      ~pieces:game_state.pieces
  in
  match next_possible_moves with
  | [] -> raise_s [%message "no possible moves!" (game_state : Game_state.t)]
  | [ single ] -> single
  | first :: (_ :: _ as rest) ->
    (* CR-someday jrodriguez: I think that if I had implemented ab-pruning
       from the start, it _could_ be cleaner. This is sooo messy. *)
    let alpha = ref (-1.0 *. Float.infinity) in
    let beta = ref Float.infinity in
    let score_of_first =
      mimimax
        ~me
        ~am_maximizing_player:false
        ~depth
        ~pieces:(Map.set game_state.pieces ~key:first ~data:me)
        ~game_kind:game_state.game_kind
        ~alpha:!alpha
        ~beta:!beta
    in
    (match Float.O.(score_of_first > !beta) with
     | true -> first
     | false ->
       alpha := Float.max !alpha score_of_first;
       List.fold_until
         rest
         ~finish:Tuple2.get1
         ~init:(first, score_of_first)
         ~f:(fun ((_, best_score) as acc) possible_move ->
         let current_score =
           mimimax
             ~me
             ~am_maximizing_player:false
             ~depth
             ~pieces:(Map.set game_state.pieces ~key:possible_move ~data:me)
             ~game_kind:game_state.game_kind
             ~alpha:!alpha
             ~beta:!beta
         in
         let best_move, best_score =
           match Float.O.(current_score > best_score) with
           | true -> possible_move, current_score
           | false -> acc
         in
         match Float.O.(best_score > !beta) with
         | true -> Stop best_move
         | false ->
           alpha := Float.max !alpha best_score;
           Continue (best_move, best_score)))
;;
