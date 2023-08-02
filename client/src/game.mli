open! Core
open! Bonsai_web
open Emb_questionbank

val component
  :  id:int Value.t
  -> set_url:(Page.t -> unit Effect.t)
  -> houses:QuestionBank.t Value.t
  -> Vdom.Node.t Computation.t
