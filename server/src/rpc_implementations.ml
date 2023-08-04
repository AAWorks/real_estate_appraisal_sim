open! Core
open! Leaderboard
open! Async
open! Tic_tac_toe_2023_common
open! Protocol

let get_rows () =
  Rpc.Rpc.implement Get_rows.rpc (fun _ (n : int) ->
    first_n_rows ~n ~filename:"resources/leaderboard.txt")
;;

let add_entry () =
  Rpc.Rpc.implement
    Add_entry.rpc
    (fun _ ((username, score) : string * int) ->
    write_row
      ~row:{ Row.username; score }
      ~filename:"resources/leaderboard.txt")
;;

let implementations () =
  Rpc.Implementations.create_exn
    ~implementations:[ get_rows (); add_entry () ]
    ~on_unknown_rpc:`Continue
;;
