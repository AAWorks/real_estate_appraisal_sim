open! Core
open! Storage
open! Async
open! Jsonaf.Export

module QuestionBank : sig
  type t

  val from_file : filename:string -> t Deferred.t
  val n_random_houses : t -> n_houses:int -> House.t list
  val to_json : t -> Jsonaf_kernel__.Type.t
end
