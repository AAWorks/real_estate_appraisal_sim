open! Core

module BetterString : sig
  val title : string -> string
  val to_price_string : int -> string
end

module House : sig
  type t [@@deriving sexp, equal]

  val int_price : t -> int
  val string_price : t -> string
  val address : t -> string
  val specs : t -> string
  val images : t -> string list
  val images_as_string : t -> string
  val bedrooms : t -> string
  val bathrooms : t -> string
  val fields : string list
end

module QuestionBank : sig
  type t = House.t list

  val from_embedded : embedded_file:string -> House.t list
  val n_random_houses : t -> n_houses:int -> House.t list
end

val questions_as_records : unit -> QuestionBank.t

val weighted_points
  :  actual:int
  -> guess:int
  -> ?point_scale:float
  -> unit
  -> int

val handle_number_input : number_str:string -> string
