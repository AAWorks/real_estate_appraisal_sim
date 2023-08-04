open! Core
open! Async
open! Async_rpc_kernel
open Tic_tac_toe_2023_common.Protocol

let read_rows ~filename =
  let%bind reader = Reader.open_file filename in
  let pipe = Reader.read_sexps reader in
  let%map sexplist = Pipe.to_list pipe in
  let rows = List.map sexplist ~f:(fun row_sexp -> Row.t_of_sexp row_sexp) in
  List.sort ~compare:(fun row row2 -> Int.compare row.score row2.score) rows
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
