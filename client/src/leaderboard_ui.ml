open! Core
open! Bonsai_web
open! Tic_tac_toe_2023_common
open! Bonsai.Let_syntax
open! Emb_questionbank
open Protocol

let rows : Row.t list Computation.t =
  let%sub { last_ok_response; _ } =
    Rpc_effect.Rpc.poll_until_ok
      ~equal_query:[%equal: int]
      Get_rows.rpc
      ~where_to_connect:Self
      ~retry_interval:(Time_ns.Span.of_sec 1.0)
      (Value.return 10)
  in
  match%sub last_ok_response with
  | None -> Bonsai.const []
  | Some (_query, response) -> return response
;;

let component ~url:_ ~set_url:_ =
  let%sub rows = rows in
  let%arr rows = rows in
  Vdom.Node.div
    [ Vdom.Node.text "Leaderboard"
    ; Vdom.Node.sexp_for_debugging [%message (rows : Row.t list)]
    ]
;;