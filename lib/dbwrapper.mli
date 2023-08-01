open! Core
open! Async
open! Postgres_async

module Row : sig
  type t =
    { username : string
    ; score : int
    }
  [@@deriving sexp, fields]
end

val create_table : unit -> unit Deferred.t
val n_rows_as_maps : n:int -> string option Core.String.Map.t list Deferred.t
val n_rows_as_records : n:int -> Row.t list Deferred.t
val insert : row:Row.t -> unit Deferred.t
