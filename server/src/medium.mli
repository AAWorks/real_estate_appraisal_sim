open! Core
open Tic_tac_toe_2023_common
open Protocol

val compute_next_move
  :  depth:int
  -> me:Piece.t
  -> game_state:Game_state.t
  -> Position.t
