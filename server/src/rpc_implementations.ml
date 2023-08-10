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

(* let world_state () = Rpc.Rpc.implement Get_world_state.rpc (fun _ (room_id
   : int) -> get_world_state ~room_id) ;;

   let create_room () = Rpc.Rpc.implement Create_room.rpc (fun _ (room_id :
   int) (player : Player.t) -> create_room ~room_id ~player) ;;

   let join_room () = Rpc.Rpc.implement Join_room.rpc (fun _ (room_id : int)
   (player : Player.t) -> join_room ~room_id ~player) ;; *)

let implementations () =
  Rpc.Implementations.create_exn
    ~implementations:[ get_rows (); add_entry () ]
    ~on_unknown_rpc:`Continue
;;
