open! Core
open! Async

val implementations
  :  global_state:State.t
  -> User_state.t Rpc.Implementations.t
