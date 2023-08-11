open! Core
open! Bonsai_web
open! Tic_tac_toe_2023_common
open! Emb_questionbank
module Form = Bonsai_web_ui_form

val component
  :  id:int Value.t
  -> room_code:int Value.t
  -> set_url:(Page.t -> unit Effect.t)
  -> houses:QuestionBank.t Value.t
  -> score:int Value.t
  -> set_score:(int -> unit Ui_effect.t) Value.t
  -> Vdom.Node.t Computation.t
