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

module Add_entry : sig
  val rpc : (string * int, unit) Async_rpc_kernel.Rpc.Rpc.t
end

module Get_rows : sig
  val rpc : (int, Row.t list) Async_rpc_kernel.Rpc.Rpc.t
end
