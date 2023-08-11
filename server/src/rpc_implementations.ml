open! Core
open! Leaderboard
open! Async
open! Tic_tac_toe_2023_common
open! Protocol

let get_rows () =
  Rpc.Rpc.implement Get_rows.rpc (fun _ (n : int) ->
    let%bind rows = first_n_rows ~n ~filename:"resources/leaderboard.txt" in
    return rows)
;;

let add_entry () =
  Rpc.Rpc.implement
    Add_entry.rpc
    (fun _ ((username, score) : string * int) ->
    write_row
      ~row:{ Row.username; score }
      ~filename:"resources/leaderboard.txt")
;;

let world_state () =
  Rpc.Rpc.implement Get_world_state.rpc (fun _ () ->
    return (RE_World_State.new_world_state ()))
;;

let create_room () =
  Rpc.Rpc.implement
    Create_room.rpc
    (fun
      _
      ((world_state, room_id, player) : RE_World_State.t * int * REPlayer.t)
    ->
    let newroom = Room.new_room ~id:room_id ~player in
    return (RE_World_State.add_room world_state ~room:newroom))
;;

let create_player () =
  Rpc.Rpc.implement Create_player.rpc (fun _ (id : int) ->
    return (REPlayer.new_player ~id))
;;

let join_room () =
  Rpc.Rpc.implement
    Join_room.rpc
    (fun
      _
      ((world_state, player, room_id) : RE_World_State.t * REPlayer.t * int)
    -> return (RE_World_State.add_to_room world_state ~room_id ~player))
;;

let implementations () =
  Rpc.Implementations.create_exn
    ~implementations:
      [ get_rows ()
      ; add_entry ()
      ; world_state ()
      ; create_room ()
      ; join_room ()
      ; create_player ()
      ]
    ~on_unknown_rpc:`Continue
;;
