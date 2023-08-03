open! Core
open! Leaderboard
open! Async
open! Tic_tac_toe_2023_common
open! Protocol

let info = Log.Global.info "%s"

let get_rows ~n : int -> Row.t =
  Rpc.Rpc.implement
    Get_rows.rpc
    (fun (user_state : User_state.t) (n : int) ->
    first_n_rows ~n ~filename:"resources/leaderboard.txt")
;;

let add_entry ~username ~score =
  Rpc.Rpc.implement Add_entry.rpc (fun (user_state : User_state.t) ->
    write_row
      ~row:{ Row.username; score }
      ~filename:"resources/leaderboard.txt")
;;

let implementations () =
  Rpc.Implementations.create_exn
    ~implementations:[ get_rows ~n; add_entry ~username ~score ]
    ~on_unknown_rpc:`Continue
;;
