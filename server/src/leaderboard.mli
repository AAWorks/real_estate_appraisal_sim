open! Core
open! Async

module Row : sig
  type t =
    { username : string
    ; score : int
    }
  [@@deriving sexp, fields]
end

val first_n_rows : n:int -> filename:string -> Row.t list Deferred.t
val write_row : row:Row.t -> filename:string -> unit Deferred.t
