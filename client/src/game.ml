open! Core
open! Bonsai_web
open! Tic_tac_toe_2023_common
open Bonsai.Let_syntax

let component ~url ~set_url:_ =
  let%arr url = url in
  Vdom.Node.div
    ~attrs:[ Style.foo ]
    [ Vdom.Node.text "Hello world!"
    ; Vdom.Node.sexp_for_debugging [%message (url : Page.t)]
    ]
;;

let component ~url ~set_url =
  Bonsai_web.View.Theme.set_for_app
    (Value.return @@ Kado.theme ~style:Dark ~version:Bleeding ())
    (component ~url ~set_url)
;;
