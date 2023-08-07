open! Core
open! Bonsai_web
open! Tic_tac_toe_2023_common
open Bonsai.Let_syntax
open! Emb_questionbank
module Form = Bonsai_web_ui_form

let find_house (id : int) houses : House.t =
  List.nth_exn houses (id % List.length houses)
;;

let right_arrow () =
  Vdom.Node.div
    ~attrs:[ Style.arrow ]
    [ Vdom.Node.div ~attrs:[ Style.arrow_top ] []
    ; Vdom.Node.div ~attrs:[ Style.arrow_bottom ] []
    ]
;;

let left_arrow () =
  Vdom.Node.div
    ~attrs:[ Style.arrow_left ]
    [ Vdom.Node.div ~attrs:[ Style.arrow_top_left ] []
    ; Vdom.Node.div ~attrs:[ Style.arrow_bottom_left ] []
    ]
;;

let house_data_card ~(id : int Value.t) (houses : QuestionBank.t Value.t) =
  let%sub house =
    let%arr id = id
    and houses = houses in
    find_house id houses
  in
  let%sub address =
    let%arr house = house in
    Emb_questionbank.House.address house
  in
  let%sub bathrooms =
    let%arr house = house in
    Emb_questionbank.House.bathrooms house
  in
  let%sub bedrooms =
    let%arr house = house in
    Emb_questionbank.House.bedrooms house
  in
  let%arr bedrooms = bedrooms
  and address = address
  and bathrooms = bathrooms in
  Vdom.Node.div
    ~attrs:[ Style.house_card ]
    [ Vdom.Node.div
        ~attrs:[ Style.house_info ]
        [ Vdom.Node.div
            ~attrs:[ Style.house_detail ]
            [ Vdom.Node.div
                ~attrs:[ Style.detail_label ]
                [ Vdom.Node.text "Location" ]
            ; Vdom.Node.div
                ~attrs:[ Style.detail_value ]
                [ Vdom.Node.text address ]
            ]
        ; Vdom.Node.div
            ~attrs:[ Style.house_detail ]
            [ Vdom.Node.div
                ~attrs:[ Style.detail_label ]
                [ Vdom.Node.text "Bedrooms:" ]
            ; Vdom.Node.div
                ~attrs:[ Style.detail_value ]
                [ Vdom.Node.text bedrooms ]
            ]
        ; Vdom.Node.div
            ~attrs:[ Style.house_detail ]
            [ Vdom.Node.div
                ~attrs:[ Style.detail_label ]
                [ Vdom.Node.text "Bathrooms:" ]
            ; Vdom.Node.div
                ~attrs:[ Style.detail_value ]
                [ Vdom.Node.text bathrooms ]
            ]
        ]
    ]
;;

let image_carousel (images : string list Value.t) =
  let%sub state, set_state = Bonsai.state 0 in
  let%arr state = state
  and set_state = set_state
  and images = images in
  let length = List.length images in
  let decrement =
    Vdom.Node.button
      ~attrs:
        [ Style.image_buttons
        ; Vdom.Attr.on_click (fun _ -> set_state (state - 1))
        ]
      [ left_arrow () ]
  in
  let increment =
    Vdom.Node.button
      ~attrs:
        [ Style.image_buttons
        ; Vdom.Attr.on_click (fun _ -> set_state (state + 1))
        ]
      [ right_arrow () ]
  in
  let selected_image = List.nth_exn images (state % length) in
  Vdom.Node.div
    ~attrs:[ Style.carousel ]
    [ decrement
    ; Vdom.Node.img
        ~attrs:
          [ Vdom.Attr.create "height" "500px"
          ; Vdom.Attr.create "width" "500px"
          ; Vdom.Attr.src selected_image
          ]
        ()
    ; increment
    ]
;;

let component
  ~id
  ~set_url
  ~(houses : QuestionBank.t Value.t)
  ~score
  ~set_score
  =
  Bonsai.scope_model (module Int) ~on:id
  @@
  let%sub playing_game =
    match%sub id with
    | 11 ->
      let%sub username_textbox =
        Form.Elements.Textbox.string ~placeholder:"Username" ()
      in
      (* let%sub username, set_username = Bonsai.state "" in *)
      let%sub sleep = Bonsai.Clock.sleep in
      let%sub send_score =
        let%sub send =
          Rpc_effect.Rpc.dispatcher
            Protocol.Add_entry.rpc
            ~where_to_connect:Self
        in
        let%arr send = send in
        fun x -> Effect.ignore_m (send x)
      in
      let%arr score = score
      and username_textbox = username_textbox
      and sleep = sleep
      and send_score = send_score in
      Vdom.Node.div
        [ (* [ Vdom.Node.button ~attrs: [ Style.button ; Vdom.Attr.on_click
             (fun _ -> set_url Page.Homepage) ] [ Vdom.Node.text "HOME" ] *)
          Vdom.Node.div
            ~attrs:[ Style.game_over; Style.big_body ]
            [ (* Vdom.Node.button ~attrs: [ Style.button ; Vdom.Attr.on_click
                 (fun _ -> set_url Page.Homepage) ] [ Vdom.Node.text "Home"
                 ] *)
              Vdom.Node.h1
                ~attrs:[ Style.second_title ]
                [ Vdom.Node.text "Property Prodigy" ]
            ; Vdom.Node.div
                ~attrs:[ Style.final_score ]
                [ Vdom.Node.text "Final Score"
                ; Vdom.Node.div
                    ~attrs:
                      [ Style.score_value
                      ; Vdom.Attr.create "font-size" "40px"
                      ]
                    [ Vdom.Node.text (Int.to_string score) ]
                ]
            ; Vdom.Node.div
                ~attrs:[ Style.input ]
                [ Form.view_as_vdom
                    ~on_submit:
                      (Form.Submit.create
                         ~handle_enter:true
                         ~f:(fun username -> send_score (username, score))
                         ())
                    username_textbox
                ]
            ; Vdom.Node.button
                ~attrs:
                  [ Style.save_button
                  ; Vdom.Attr.on_click (fun _ ->
                      let%bind.Effect () = sleep (Time_ns.Span.of_sec 1.5) in
                      Effect.return ())
                  ]
                [ Vdom.Node.text "Save Score" ]
            ]
        ]
    | _ ->
      let%sub guess, set_guess = Bonsai.state None in
      let%sub photo_carousel =
        let%sub images =
          let%arr id = id
          and houses = houses in
          let selected_house = find_house id houses in
          Emb_questionbank.House.images selected_house
        in
        image_carousel images
      in
      let%sub guess_screen =
        match%sub guess with
        | None ->
          let%sub textbox =
            Form.Elements.Textbox.int ~placeholder:"PRICE" ()
          in
          let%sub sleep = Bonsai.Clock.sleep in
          let%sub int_house_price =
            let%sub selected_house =
              let%arr id = id
              and houses = houses in
              find_house id houses
            in
            let%arr selected_house = selected_house in
            Emb_questionbank.House.int_price selected_house
          in
          let%sub theme =
            let%sub theme = View.Theme.current in
            let%arr theme = theme in
            View.Expert.override_theme theme ~f:(fun (module S) ->
              (module struct
                class c =
                  object
                    inherit S.c
                  end
              end))
          in
          View.Theme.set_for_computation theme
          @@
          let%arr textbox = textbox
          and set_guess = set_guess
          and score = score
          and set_score = set_score
          and id = id
          and sleep = sleep
          and int_house_price = int_house_price in
          Vdom.Node.div
            ~attrs:[ Style.input ]
            [ (Form.view_as_vdom
                 ~on_submit:
                   (Form.Submit.create
                      ~handle_enter:true
                      ~f:(fun guess ->
                        let%bind.Effect () = set_guess (Some guess) in
                        let%bind.Effect () =
                          set_score
                            (score
                             + Emb_questionbank.weighted_points
                                 ~actual:int_house_price
                                 ~guess
                                 ())
                        in
                        let%bind.Effect () =
                          sleep (Time_ns.Span.of_sec 3.0)
                        in
                        set_url (Page.Game (id + 1)))
                      ())
                 textbox
               |> fun x ->
               let attr = Vdom.Attr.create "autocomplete" "off" in
               match x with
               | Vdom.Node.Element element ->
                 Vdom.Node.Element
                   (Vdom.Node.Element.map_attrs element ~f:(fun a ->
                      Vdom.Attr.combine a attr))
               | x -> Vdom.Node.span ~attrs:[ attr ] [ x ])
            ]
        | Some guess ->
          let%sub string_house_price =
            let%sub selected_house =
              let%arr id = id
              and houses = houses in
              find_house id houses
            in
            let%arr selected_house = selected_house in
            Emb_questionbank.House.string_price selected_house
          in
          let%sub int_house_price =
            let%sub selected_house =
              let%arr id = id
              and houses = houses in
              find_house id houses
            in
            let%arr selected_house = selected_house in
            Emb_questionbank.House.int_price selected_house
          in
          let%sub awarded_points =
            let%arr house_price = int_house_price
            and guess = guess in
            Emb_questionbank.weighted_points ~actual:house_price ~guess ()
          in
          let%arr guess = guess
          and house_price = string_house_price
          and awarded_points = awarded_points in
          Vdom.Node.div
            ~attrs:[ Style.outside ]
            [ Vdom.Node.div
                ~attrs:[ Style.guessing_container ]
                [ Vdom.Node.div
                    ~attrs:[ Style.house_price ]
                    [ Vdom.Node.div
                        ~attrs:[ Style.label ]
                        [ Vdom.Node.text "Actual Price:" ]
                    ; Vdom.Node.div
                        ~attrs:[ Style.value ]
                        [ Vdom.Node.text house_price ]
                    ]
                ; Vdom.Node.div
                    ~attrs:[ Style.user_guess ]
                    [ Vdom.Node.div
                        ~attrs:[ Style.label ]
                        [ Vdom.Node.text "Your Guess:" ]
                    ; Vdom.Node.div
                        ~attrs:[ Style.value ]
                        [ Vdom.Node.text (BetterString.to_price_string guess)
                        ]
                    ]
                ; Vdom.Node.div
                    [ Vdom.Node.div
                        ~attrs:[ Style.result ]
                        [ Vdom.Node.text "Score Earned:" ]
                    ; Vdom.Node.div
                        ~attrs:[ Style.score_value ]
                        [ Vdom.Node.text ("+" ^ Int.to_string awarded_points)
                        ]
                    ]
                ]
            ]
      in
      let%sub house_data = house_data_card ~id houses in
      let%arr guess_screen = guess_screen
      and photo_carousel = photo_carousel
      and house_data = house_data
      and score = score in
      Vdom.Node.div
        ~attrs:[ Style.big_body ]
        [ Vdom.Node.h1
            ~attrs:[ Style.second_title ]
            [ Vdom.Node.text "Property Prodigy" ]
          (* ; Vdom.Node.button ~attrs: [ Style.button ; Vdom.Attr.on_click
             (fun _ -> set_url Page.Homepage) ] [ Vdom.Node.text "Go to
             homepage" ] *)
        ; Vdom.Node.div
            (* ~attrs:[ Style.organize ] *)
            [ Vdom.Node.div
                ~attrs:[ Style.top_row ]
                [ Vdom.Node.div
                    ~attrs:[ Style.score_card ]
                    [ Vdom.Node.div
                        ~attrs:[ Style.current_score ]
                        [ Vdom.Node.text "Current Score" ]
                    ; Vdom.Node.div
                        ~attrs:[ Style.user_score ]
                        [ Vdom.Node.text (Int.to_string score) ]
                    ]
                ; house_data
                ]
            ; photo_carousel
            ; guess_screen
            ]
        ]
  in
  let%arr playing_game = playing_game in
  Vdom.Node.div [ playing_game ]
;;
