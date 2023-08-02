open! Core
open! Async
open Tic_tac_toe_2023_common
open Protocol

(** Represents the overall state of the application. *)

type t

val to_string_hum : t -> string
val create : world_state:World_state.t -> t

val create_game
  :  t
  -> first_player:Username.t
  -> query:Create_game.Query.t
  -> Game_id.t

val join_game
  :  t
  -> second_player:Username.t
  -> game_id:Game_id.t
  -> Join_existing_game.Response.t

val get_all_running_games : t -> Game_state.t Game_id.Map.t
val get_all_joinable_games : t -> Joinable_game.t Game_id.Map.t
val get_game : t -> game_id:Game_id.t -> Get_game.Response.t

val take_turn
  :  t
  -> game_id:Game_id.t
  -> position:Position.t
  -> username:Username.t
  -> Take_turn.Response.t Deferred.t

val is_thinking : t -> game_id:Game_id.t -> bool
