open! Core
open! Bonsai_web
open! Tic_tac_toe_2023_common
open Bonsai.Let_syntax
open! Emb_questionbank

let get_houses () = questions_as_records ()

let component ~url ~set_url =
  match%sub url with
  | Page.Homepage ->
    Bonsai.const
    @@ Vdom.Node.div
         ~attrs:[]
         [ Vdom.Node.h1
             ~attrs:[ Style.title ]
             [ Vdom.Node.text "Property Prodigy" ]
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
    let%sub house_state, set_houses = Bonsai.state (get_houses ()) in
    let%sub out, reset =
      Bonsai.with_model_resetter
        (Game.component ~id ~set_url ~houses:house_state)
    in
    (* Put houses in compoent *)
    let%sub () = Bonsai.Edge.lifecycle ~on_activate:reset () in
    let%sub () =
      Bonsai.Edge.lifecycle
        ()
        ~on_activate:
          (let%map set_houses = set_houses in
           set_houses (get_houses ()))
    in
    return out
;;
