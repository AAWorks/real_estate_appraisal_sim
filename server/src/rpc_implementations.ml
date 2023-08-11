open! Core
open! Leaderboard
open! Async
open! Tic_tac_toe_2023_common
open! Protocol

type t = { world_state : RE_World_State.t }

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

let world_state t =
  Rpc.Rpc.implement Get_world_state.rpc (fun _ () -> return t.world_state)
;;

let create_room t =
  Rpc.Rpc.implement
    Create_room.rpc
    (fun
        _
        ((room_id, player) :
          int * REPlayer.t)
      ->
      let newroom = Room.new_room ~id:room_id ~player in
      return (RE_World_State.add_room t.world_state ~room:newroom))
;;

let create_player () =
  Rpc.Rpc.implement Create_player.rpc (fun _ (id : int) ->
    return (REPlayer.new_player ~id))
;;

let join_room t =
  Rpc.Rpc.implement
    Join_room.rpc
    (fun
        _
        ((player, room_id) :
          REPlayer.t * int)
      -> return (RE_World_State.add_to_room t.world_state ~room_id ~player))
;;

let get_room t = 
  Rpc.Rpc.implement
    Get_room.rpc
    (fun 
    _
    (room_id : int)
    -> return (RE_World_State.get_room t.world_state ~room_id))

let create () = { world_state = RE_World_State.new_world_state () }

let implementations () =
  let t = create () in
  Rpc.Implementations.create_exn
    ~implementations:
      [ get_rows ()
      ; add_entry ()
      ; world_state t
      ; create_room t
      ; join_room t
      ; create_player ()
      ; get_room t
      ]
    ~on_unknown_rpc:`Continue
;;
