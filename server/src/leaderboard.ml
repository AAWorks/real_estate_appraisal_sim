open! Core
open! Async
open! Async_rpc_kernel
open Tic_tac_toe_2023_common.Protocol

let read_rows ~filename =
  let%bind reader = Reader.open_file filename in
  let pipe = Reader.read_sexps reader in
  let%map sexplist = Pipe.to_list pipe in
  let rows = List.map sexplist ~f:(fun row_sexp -> Row.t_of_sexp row_sexp) in
  List.sort ~compare:(fun row row2 -> Int.compare row2.score row.score) rows
;;

let first_n_rows ~n ~filename =
  let%bind rows = read_rows ~filename in
  return (List.take rows n)
;;

let write_row ~row ~filename =
  let%bind rows = read_rows ~filename in
  let rowsexps =
    List.map (rows @ [ row ]) ~f:(fun irow -> Row.sexp_of_t irow)
  in
  Writer.save_sexps "resources/leaderboard.txt" rowsexps
;;

let write_example_rows () =
  let%bind rows = read_rows ~filename:"resources/leaderboard.txt" in
  let to_add =
    [ { Row.username = "a"; score = 1000 }
    ; { Row.username = "b"; score = 900 }
    ; { Row.username = "c"; score = 800 }
    ; { Row.username = "d"; score = 700 }
    ; { Row.username = "e"; score = 600 }
    ; { Row.username = "f"; score = 500 }
    ; { Row.username = "g"; score = 400 }
    ; { Row.username = "h"; score = 300 }
    ; { Row.username = "i"; score = 200 }
    ; { Row.username = "j"; score = 100 }
    ]
  in
  let sexps_to_add =
    List.map (rows @ to_add) ~f:(fun irow -> Row.sexp_of_t irow)
  in
  Writer.save_sexps "resources/leaderboard.txt" sexps_to_add
;;
