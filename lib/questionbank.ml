open! Core
open! Storage
open! Async
open! Jsonaf.Export

module Houses = struct
  type t = House.t list [@@deriving sexp] [@@jsonaf.allow_extra_fields]
end

module QuestionBank = struct
  type t = House.t list Deferred.t

  let from_file ~filename : t =
    let%bind reader = Reader.open_file filename in
    let pipe = Reader.read_sexps reader in
    let%map sexplist = Pipe.to_list pipe in
    List.map sexplist ~f:(fun house_sexp -> House.t_of_sexp house_sexp)
  ;;

  let n_random_houses (t : Houses.t) ~n_houses =
    let shuffled_houses = List.permute t in
    List.take shuffled_houses n_houses
  ;;

  (* let to_json (t : Houses.t) = jsonaf_of_t t *)
end

let _get_questions () =
  let%bind questionbank =
    QuestionBank.from_file ~filename:"resources/house_data.txt"
  in
  return
    (QuestionBank.n_random_houses
       questionbank
       ~n_houses:10 (* |> QuestionBank.to_json) *))
;;
