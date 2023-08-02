open! Core
open! Bonsai_web
open! Tic_tac_toe_2023_common
open Bonsai.Let_syntax
open! Emb_questionbank
module Form = Bonsai_web_ui_form

let find_house (id : int) houses : House.t =
  List.nth_exn houses (id % List.length houses)
;;

let _state_based_counter length =
  let%sub state, set_state = Bonsai.state 0 in
  let%arr state = state
  and set_state = set_state in
  let decrement =
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.on_click (fun _ ->
            if not (Int.equal state 0)
            then set_state (state - 1)
            else set_state state)
        ]
      [ Vdom.Node.text "<-" ]
  in
  let increment =
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.on_click (fun _ ->
            if not (Int.equal state length)
            then set_state (state + 1)
            else set_state state)
        ]
      [ Vdom.Node.text "->" ]
  in
  increment, decrement
;;

let component ~id ~set_url ~(houses : QuestionBank.t Value.t) =
  Bonsai.scope_model (module Int) ~on:id
  @@
  let%sub guess, set_guess = Bonsai.state None in
  let%sub rendered_house =
    let%arr id = id
    and houses = houses in
    let selected_house = find_house id houses in
    let images = Emb_questionbank.House.images selected_house in
    Vdom.Node.sexp_for_debugging [%sexp (images : string list)]
  in
  (* let%sub carosel_buttons =
    let%arr id = id
    and houses = houses in
    let selected_house = find_house id houses in
    let images = Emb_questionbank.House.images selected_house in
    let total_images = List.length images in
    Vdom.Node.view_as_vdom (state_based_counter total_images)
  in *)
  (* Vdom.Node.sexp_for_debugging [%sexp (find_house id houses : House.t)] *)
  let%sub guess_screen =
    match%sub guess with
    | None ->
      let%sub textbox = Form.Elements.Textbox.int () in
      let%sub sleep = Bonsai.Clock.sleep in
      let%arr textbox = textbox
      and set_guess = set_guess
      and id = id
      and sleep = sleep in
      Vdom.Node.div
        ~attrs:[ Style.input; Vdom.Attr.placeholder "PRICE" ]
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
        ~attrs:[ Style.second_title ]
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
