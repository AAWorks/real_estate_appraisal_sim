open! Core
open! Async

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
