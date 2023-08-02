open! Core
open! Async
open! Postgres_async

module Row = struct
  type t =
    { username : string
    ; score : int
    }
  [@@deriving sexp, fields]
end

let with_connection_exn =
  let database = "test_query" in
  let harness =
    lazy
      (let h = Harness.create () in
       Harness.create_database h database;
       h)
  in
  fun func -> Harness.with_connection_exn (force harness) ~database func
;;

let read_query_exn postgres ?handle_columns ?parameters ?pushback str =
  let handle_row ~column_names ~values = Array.zip_exn column_names values in
  let pipe =
    Pipe.create_reader
      ~size_budget:1000
      ~close_on_exception:false
      (fun writer ->
      let%bind res =
        Postgres_async.query
          postgres
          ?handle_columns
          ?parameters
          ?pushback
          str
          ~handle_row:(fun ~column_names ~values ->
          Pipe.write_without_pushback
            writer
            (handle_row ~column_names ~values))
      in
      Or_error.ok_exn res;
      return ())
  in
  Pipe.to_list pipe
;;

let write_query_exn postgres ?parameters str =
  let%bind res =
    Postgres_async.query_expect_no_data postgres ?parameters str
  in
  Or_error.ok_exn res;
  return ()
;;

let create_table () =
  with_connection_exn (fun postgres ->
    let query_exn = write_query_exn postgres in
    query_exn
      "CREATE TABLE IF NOT EXISTS a ( entryid SERIAL PRIMARY KEY, username \
       TEXT, score INTEGER );")
;;

let get_rows () =
  let ivar = Ivar.create () in
  let%bind () =
    with_connection_exn (fun postgres ->
      let query_exn = read_query_exn postgres in
      let%bind leaderboard =
        query_exn "SELECT * FROM leaderboard ORDER BY score DESC"
      in
      Ivar.fill_if_empty ivar leaderboard;
      return ())
  in
  Ivar.read ivar
;;

let n_rows_as_maps ~n =
  let%bind all_rows = get_rows () in
  let nrows =
    List.take all_rows n
    |> List.map ~f:(fun row -> String.Map.of_alist_exn (List.of_array row))
  in
  return nrows
;;

let n_rows_as_records ~n =
  let%bind n_rows = n_rows_as_maps ~n in
  let recordlist =
    List.map n_rows ~f:(fun row ->
      { Row.username = Map.find_exn row "username" |> Option.value_exn
      ; score = Map.find_exn row "score" |> Option.value_exn |> Int.of_string
      })
  in
  return recordlist
;;

let insert ~(row : Row.t) =
  with_connection_exn (fun postgres ->
    let query_exn = write_query_exn postgres in
    let query_str =
      Core.Printf.sprintf
        "INSERT INTO leaderboard (username, score) VALUES (%s, %d)"
        row.username
        row.score
    in
    query_exn query_str)
;;
