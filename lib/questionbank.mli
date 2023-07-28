open! Core
open! Storage
open! Jsonaf.Export

module Houses : sig
  type t [@@deriving sexp] [@@jsonaf.allow_extra_fields]
end

module QuestionBank : sig
  type t

  val from_file : filename:string -> t
  val n_random_houses : Houses.t -> n_houses:int -> House.t list
  (* val to_json : t -> Jsonaf_kernel__.Type.t *)
end
