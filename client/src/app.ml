open! Core
open! Bonsai_web
open! Tic_tac_toe_2023_common
open Bonsai.Let_syntax

let component ~url ~set_url =
  match%sub url with
  | Page.Homepage ->
    let%arr url = url in
    Vdom.Node.div
      ~attrs:[]
      [ Vdom.Node.h1
          ~attrs:[ Style.title ]
          [ Vdom.Node.text "Property Prodigy" ]
      ; Vdom.Node.sexp_for_debugging [%message (url : Page.t)]
      ; Vdom.Node.div
          ~attrs:[ Style.help ]
          [ Vdom.Node.button
              ~attrs:
                [ Style.button
                ; Vdom.Attr.on_click (fun _ -> set_url (Page.Game 1))
                ]
              [ Vdom.Node.text "Start" ]
          ; Vdom.Node.button
              ~attrs:
                [ Style.button
                ; Vdom.Attr.on_click (fun _ -> set_url (Page.Game 2))
                ]
              [ Vdom.Node.text "Leaderboard" ]
          ]
      ]
  | Game id ->
    let%sub out, reset =
      Bonsai.with_model_resetter (Game.component ~id ~set_url)
    in
    (* Put houses in compoent *)
    let%sub () = Bonsai.Edge.lifecycle ~on_activate:reset () in
    return out
;;
