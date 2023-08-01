open! Core
open! Bonsai_web
open! Tic_tac_toe_2023_common
open Bonsai.Let_syntax
open Emb_questionbank
module Form = Bonsai_web_ui_form

(* let houses = Questionbank.questions_as_records;; *)
(* List of House.t *)

(* let houses = Embedded_files.house_data_dot_txt |> [%sexp_of: StoHouse.t

   list] *)

let houses = questions_as_records ()

let find_house : int -> House.t =
  fun i -> List.nth_exn houses (i % List.length houses)
;;

let component ~id ~set_url =
  Bonsai.scope_model (module Int) ~on:id
  @@
  let%sub guess, set_guess = Bonsai.state None in
  let%sub rendered_house =
    let%arr id = id in
    Vdom.Node.sexp_for_debugging [%sexp (find_house id : House.t)]
  in
  let%sub guess_screen =
    match%sub guess with
    | None ->
      (* Text box*)
      let%sub textbox = Form.Elements.Textbox.int () in
      let%sub sleep = Bonsai.Clock.sleep in
      let%arr textbox = textbox >>| Form.label "testing textbox"
      and set_guess = set_guess
      and id = id
      and sleep = sleep in
      Vdom.Node.div
        [ Form.view_as_vdom
            ~on_submit:
              (Form.Submit.create
                 ~handle_enter:true
                 ~f:(fun guess ->
                   let%bind.Effect () = set_guess (Some guess) in
                   let%bind.Effect () = sleep (Time_ns.Span.of_sec 3.0) in
                   set_url (Page.Game (id + 1)))
                 ())
            textbox
        ]
    | Some guess ->
      (* val score : guess:int -> game_id:int -> int *)

      (* Scoring.weighted_points ~actual:(Storage.House.int_price house)
         ~guess *)
      let%arr guess = guess in
      let actual =
        Vdom.Node.text "Actual price: "
        (* Storage.House.string_price house *)
      in
      let awarded_points = 3 in
      let _ = guess in
      let guess =
        Vdom.Node.text
          ("You Guessed: "
           ^ "" (*Storage.BetterString.to_price_string guess*))
      in
      let points =
        Vdom.Node.text ("Points:" ^ Int.to_string awarded_points)
      in
      Vdom.Node.div [ actual; guess; points ]
  in
  let%arr guess_screen = guess_screen
  and rendered_house = rendered_house in
  Vdom.Node.div
    [ Vdom.Node.h1
        ~attrs:[ Style.title ]
        [ Vdom.Node.text "Property Prodigy" ]
    ; rendered_house
    ; Vdom.Node.button
        ~attrs:
          [ Style.button
          ; Vdom.Attr.on_click (fun _ -> set_url Page.Homepage)
          ]
        [ Vdom.Node.text "Go to homepage" ]
    ; guess_screen
    ]
;;
