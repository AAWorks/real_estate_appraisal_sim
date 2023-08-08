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

let component ~url:_ ~set_url =
  let%sub rows = rows in
  let%arr rows = rows in
  Vdom.Node.div
    ([ Vdom.Node.h1
         ~attrs:[ Style.title ]
         [ Vdom.Node.text "Property Prodigy Leaderboard" ]
     ]
     @ [ Vdom.Node.ol
           ~attrs:[ Leaderboard.myol ]
           (List.map rows ~f:(fun row ->
              Vdom.Node.li
                ~attrs:[ Leaderboard.myli ]
                [ Vdom.Node.div
                    ~attrs:[ Leaderboard.flex ]
                    [ Vdom.Node.div ~attrs:[] [ Vdom.Node.text row.username ]
                    ; Vdom.Node.div
                        ~attrs:[]
                        [ Vdom.Node.text ("Score: " ^ Int.to_string row.score)
                        ]
                    ]
                ]))
       ]
     @ [ Vdom.Node.div
           ~attrs:[ Leaderboard.leaderboard_home ]
           [ Vdom.Node.button
               ~attrs:
                 [ Style.save_button
                 ; Leaderboard.leaderboard_home
                 ; Vdom.Attr.on_click (fun _ -> set_url Page.Homepage)
                 ]
               [ Vdom.Node.text "HOME" ]
           ]
       ])
;;
