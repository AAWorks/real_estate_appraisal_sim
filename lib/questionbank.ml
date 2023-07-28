open! Core
open! Storage
open! Async
open! Jsonaf
open! Jsonaf.Export

module QuestionBank = struct
  type t = House.t list [@@deriving jsonaf]

  let from_file ~filename : t Deferred.t =
    let%bind reader = Reader.open_file filename in
    let pipe = Reader.read_sexps reader in
    let%map sexplist = Pipe.to_list pipe in
    List.map sexplist ~f:(fun house_sexp -> House.t_of_sexp house_sexp)
  ;;

  let n_random_houses (t : House.t list) ~n_houses : t =
    let shuffled_houses = List.permute t in
    List.take shuffled_houses n_houses
  ;;

  let to_json (t : t) = jsonaf_of_t t
end

let questions_as_records () =
  let%bind questionbank =
    QuestionBank.from_file ~filename:"resources/house_data.txt"
  in
  return (QuestionBank.n_random_houses questionbank ~n_houses:10)
;;

let questions_as_json () =
  let%bind questionbank =
    QuestionBank.from_file ~filename:"resources/house_data.txt"
  in
  return
    (QuestionBank.n_random_houses questionbank ~n_houses:10
     |> QuestionBank.to_json)
;;
