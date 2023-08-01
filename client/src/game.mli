open! Core
open! Bonsai_web

val component
  :  id:int Value.t
  -> set_url:(Page.t -> unit Effect.t)
  -> Vdom.Node.t Computation.t
