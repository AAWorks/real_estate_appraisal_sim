open! Core
open! Bonsai_web
open! Tic_tac_toe_2023_common
open Bonsai.Let_syntax
open! Emb_questionbank

let get_houses () = questions_as_records ()

let _world_state () =
  let%map.Bonsai.Computation { last_ok_response; _ } =
    Rpc_effect.Rpc.poll_until_ok
      ~equal_query:(fun () () -> true)
      Protocol.Get_world_state.rpc
      ~retry_interval:(Time_ns.Span.of_sec 1.0)
      ~where_to_connect:Self
      (Value.return ())
  in
  match last_ok_response with
  | Some (_query, response) -> response
  | None -> { room_map = Int.Map.empty }
;;

let component ~url ~set_url =
  match%sub url with
  | Page.Leaderboard -> Leaderboard_ui.component ~url ~set_url
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
                 [ Vdom.Node.text "Single Player" ]
             ; Vdom.Node.button
                 ~attrs:
                   [ Style.button
                   ; Vdom.Attr.on_click (fun _ ->
                       set_url (Page.Multiplayer 1))
                   ]
                 [ Vdom.Node.text "Multiplayer" ]
             ; Vdom.Node.button
                 ~attrs:
                   [ Style.button
                   ; Vdom.Attr.on_click (fun _ -> set_url Page.Leaderboard)
                   ]
                 [ Vdom.Node.text "Leaderboard" ]
             ]
         ]
  | Multiplayer id ->
    let%sub room_code, set_room_code =
      Bonsai.state (Random.int 899999 + 100000)
    in
    let%sub house_state, set_houses = Bonsai.state (get_houses ()) in
    let%sub out, reset =
      Bonsai.with_model_resetter
        (let%sub score, set_score = Bonsai.state 0 in
         Multiplayer_ui.component
           ~id
           ~room_code
           ~set_url
           ~houses:house_state
           ~score
           ~set_score)
    in
    (* Put houses in compoent *)
    let%sub () = Bonsai.Edge.lifecycle ~on_activate:reset () in
    let%sub () =
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%map set_room_code = set_room_code in
           set_room_code (Random.int 899999 + 100000))
        ()
    in
    let%sub () =
      Bonsai.Edge.lifecycle
        ()
        ~on_activate:
          (let%map set_houses = set_houses in
           set_houses (get_houses ()))
    in
    return out
  | Game id ->
    let%sub house_state, set_houses = Bonsai.state (get_houses ()) in
    let%sub out, reset =
      Bonsai.with_model_resetter
        (let%sub score, set_score = Bonsai.state 0 in
         Game.component ~id ~set_url ~houses:house_state ~score ~set_score)
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
