open! Core
open! Async

module BetterString : sig
  val title : string -> string
end

module House : sig
  type t [@@deriving sexp, equal, jsonaf] [@@jsonaf.allow_extra_fields]

  val string_price : t -> string
  val address : t -> string
  val specs : t -> string
  val images : t -> string list
  val images_as_string : t -> string
  val fields : string list

  val from_scraped_data
    :  mapped_details:(string, string, 'a) Map_intf.Map.t
    -> images:string list
    -> t
end

val pull_data : unit -> unit Deferred.t
