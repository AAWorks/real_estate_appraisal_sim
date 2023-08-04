open! Core
open! Async
open Tic_tac_toe_2023_common.Protocol

val first_n_rows : n:int -> filename:string -> Row.t list Deferred.t
val write_row : row:Row.t -> filename:string -> unit Deferred.t
