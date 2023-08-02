open! Core
open! Async
open Tic_tac_toe_2023_common
open Protocol

module Traversal_result : sig
  type t =
    | Win of { winning_positions : Position.Set.t }
    | Saw_consecutive of { n : int }
end

val traverse
  :  n:int
  -> pieces:Piece.t Position.Map.t
  -> expected_piece:Piece.t
  -> position:Position.t
  -> compute_next_position:(Position.t -> Position.t)
  -> game_kind:Game_kind.t
  -> seen_positions:Position.Set.t
  -> Traversal_result.t

val did_piece_win
  :  piece:Piece.t
  -> game_kind:Game_kind.t
  -> Piece.t Position.Map.t
  -> Position.Set.t option

val score_for_piece
  :  piece:Piece.t
  -> game_kind:Game_kind.t
  -> Piece.t Position.Map.t
  -> float
