open! Core
open! Async
open Tic_tac_toe_2023_common
open Protocol

module Traversal_result = struct
  type t =
    | Win of { winning_positions : Position.Set.t }
    | Saw_consecutive of { n : int }
end

let rec traverse
  ~n
  ~pieces
  ~expected_piece
  ~position
  ~compute_next_position
  ~game_kind
  ~seen_positions
  : Traversal_result.t
  =
  let saw_consecutive =
    lazy (Traversal_result.Saw_consecutive { n = Set.length seen_positions })
  in
  match n with
  | 0 -> Traversal_result.Win { winning_positions = seen_positions }
  | _ ->
    (match Position.in_bounds ~game_kind position with
     | false -> force saw_consecutive
     | true ->
       (match Map.find pieces position with
        | None -> force saw_consecutive
        | Some board_piece ->
          (match Piece.equal board_piece expected_piece with
           | false -> force saw_consecutive
           | true ->
             let next_position = compute_next_position position in
             let seen_positions = Set.add seen_positions position in
             traverse
               ~game_kind
               ~n:(n - 1)
               ~pieces
               ~expected_piece
               ~compute_next_position
               ~position:next_position
               ~seen_positions)))
;;

let did_piece_win
  ~(piece : Piece.t)
  ~(game_kind : Game_kind.t)
  (pieces : Piece.t Position.Map.t)
  : Position.Set.t option
  =
  let win_length = Game_kind.win_length game_kind in
  List.find_map (Map.keys pieces) ~f:(fun position ->
    List.find_map
      [ Position.right
      ; Position.down
      ; Fn.compose Position.down Position.right
      ; Fn.compose Position.up Position.right
      ]
      ~f:(fun compute_next_position ->
        match
          traverse
            ~n:win_length
            ~pieces
            ~expected_piece:piece
            ~position
            ~compute_next_position
            ~game_kind
            ~seen_positions:Position.Set.empty
        with
        | Saw_consecutive _ -> None
        | Win { winning_positions } -> Some winning_positions))
;;

let score_for_piece
  ~(piece : Piece.t)
  ~(game_kind : Game_kind.t)
  (pieces : Piece.t Position.Map.t)
  : float
  =
  let win_length = Game_kind.win_length game_kind in
  List.fold (Map.keys pieces) ~init:0.0 ~f:(fun acc position ->
    List.fold
      [ Position.right
      ; Position.down
      ; Fn.compose Position.down Position.right
      ; Fn.compose Position.up Position.right
      ]
      ~init:acc
      ~f:(fun acc compute_next_position ->
        match Float.is_inf acc with
        | true -> acc
        | false ->
          (match
             traverse
               ~n:win_length
               ~pieces
               ~expected_piece:piece
               ~position
               ~compute_next_position
               ~game_kind
               ~seen_positions:Position.Set.empty
           with
           | Saw_consecutive { n } ->
             let n = Int.to_float n in
             acc +. (n *. n)
           | Win { winning_positions = _ } -> Float.infinity)))
;;
