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

val questions_as_records : unit -> House.t list Deferred.t

val questions_as_json
  :  unit
  -> Jsonaf_kernel.t Async_kernel__Types.Deferred.t
