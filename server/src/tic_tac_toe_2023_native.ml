open! Core
open! Async
open Tic_tac_toe_2023_common
open Protocol

let initialize_connection
  :  unit -> Rpc_websocket.Rpc.Connection_initiated_from.t
  -> Socket.Address.Inet.t -> Rpc.Connection.t -> 's
  =
 fun () _initiated_from inet connection ->
  let user =
    let address = Socket.Address.to_string inet in
    match String.split ~on:':' address with
    | [ ip_address; _port ] -> ip_address
    | [] | [ _ ] | _ :: _ :: _ :: _ -> address
  in
  print_s [%message "user joined" (user : string)];
  { User_state.user; connection }
;;

let main ~js_file ~port =
  let _global_state = State.create ~world_state:World_state.empty in
  let%bind server =
    let open Cohttp_static_handler in
    let javascript =
      Asset.local
        Asset.Kind.javascript
        (Asset.What_to_serve.file ~path:js_file)
    in
    let sourcemap_file =
      String.chop_suffix_exn js_file ~suffix:".js" ^ ".map"
    in
    let sourcemap =
      Asset.local
        Asset.Kind.sourcemap
        (Asset.What_to_serve.file ~path:sourcemap_file)
    in
    let http_handler () =
      Core.print_s [%message "handling..."];
      Single_page_handler.create_handler
        (Single_page_handler.default_with_body_div ~div_id:"app")
        ~assets:[ javascript; sourcemap ]
        ~title:"Tictactoe"
        ~on_unknown_url:`Index
    in
    Rpc_websocket.Rpc.serve
      ~on_handler_error:`Ignore
      ~mode:`TCP
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ~http_handler
      ~implementations:(Rpc_implementations.implementations ())
      ~initial_connection_state:initialize_connection
      ()
  in
  print_endline [%string "Game server running on port %{port#Int}"];
  Cohttp_async.Server.close_finished server
;;

let command =
  Command.async
    ~summary:"Start server for example [starter_template]"
    (let%map_open.Command port =
       flag
         "port"
         (optional_with_default 8080 int)
         ~doc:"port on which to serve"
     and js_file =
       flag
         "js-file"
         (required Filename_unix.arg_type)
         ~doc:
           "FILENAME The path to the JavaScript file which is served by the \
            web server"
     in
     fun () -> main ~js_file ~port)
;;

module For_testing = struct
  module State = State
end
