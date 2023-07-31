open! Core
open Tic_tac_toe_2023_common
open Protocol

let empty_pieces ~game_state =
  let length = Game_kind.board_length game_state.Game_state.game_kind in
  let all_positions =
    let%bind.List row = List.init length ~f:Fn.id in
    let%map.List column = List.init length ~f:Fn.id in
    { Position.row; column }
  in
  List.filter all_positions ~f:(fun position ->
    not (Map.mem game_state.pieces position))
;;

let compute_next_move ~me:(_ : Piece.t) ~(game_state : Game_state.t)
  : Position.t
  =
  List.random_element_exn
    ~random_state:(Core.Random.State.make [| 42 |])
    (empty_pieces ~game_state)
;;
