open! Core
open! Bonsai_web
open! Tic_tac_toe_2023_common
module Form = Bonsai_web_ui_form

module BetterString = struct
  include String

  let title str =
    str
    |> String.split ~on:' '
    |> List.map ~f:(fun word -> String.capitalize word)
    |> String.concat ~sep:" "
  ;;

  let to_price_string n =
    "$"
    ^ (n |> Printf.sprintf "%#d" |> String.tr ~target:'_' ~replacement:',')
  ;;

  let int_of_price_string n =
    n
    |> String.filter ~f:(fun ch ->
      not (Char.equal ch ',' || Char.equal ch '$'))
  ;;
end

module House = struct
  type t =
    { zpid : string
    ; city : string
    ; state : string
    ; bedrooms : string
    ; bathrooms : string
    ; price : string
    ; images : string list
    }
  [@@deriving sexp, equal]

  let int_price t = t.price |> Float.of_string |> Int.of_float
  let string_price t = BetterString.to_price_string (int_price t)

  let address t =
    BetterString.title (Uri.pct_decode t.city)
    ^ ", "
    ^ (String.tr ~target:'"' ~replacement:' ' t.state |> String.strip)
  ;;

  let specs t = t.bedrooms ^ " bed, " ^ t.bathrooms ^ " bath"
  let bedrooms t = t.bedrooms
  let bathrooms t = t.bathrooms

  let images t =
    List.map t.images ~f:(fun url ->
      String.tr ~target:'"' ~replacement:' ' url |> String.strip)
  ;;

  let images_as_string t = String.concat ~sep:", " (images t)
  let fields = [ "zpid"; "city"; "state"; "bedrooms"; "bathrooms"; "price" ]
end

module QuestionBank = struct
  type t = House.t list

  let from_embedded ~embedded_file =
    embedded_file |> Sexp.of_string_many |> List.map ~f:[%of_sexp: House.t]
  ;;

  let n_random_houses (t : House.t list) ~n_houses : t =
    let shuffled_houses = List.permute t in
    List.take shuffled_houses n_houses
  ;;
end

let questions_as_records () =
  let questionbank =
    QuestionBank.from_embedded
      ~embedded_file:Embedded_files.house_data_dot_txt
  in
  QuestionBank.n_random_houses questionbank ~n_houses:10
;;

let weighted_points ~actual ~guess ?(point_scale = 100.0) () =
  let pct = guess // actual in
  if Float.O.(pct > 1.0)
  then
    if Float.O.(pct > 2.0)
    then 0
    else Int.of_float ((1.0 -. pct +. 1.0) *. point_scale)
  else Int.of_float (pct *. point_scale)
;;

let handle_number_input ~(number_str : string) : string =
  let number_str =
    String.filter ~f:(function '$' | ',' -> false | _ -> true) number_str
  in
  let check_str ~s =
    try String.equal (Int.of_string s |> Int.to_string) s with
    | Failure _ -> false
  in
  if check_str ~s:number_str
  then BetterString.to_price_string (Int.of_string number_str)
  else number_str
;;

let%expect_test _ =
  let test s =
    let res = handle_number_input ~number_str:s in
    print_endline [%string "%{s} -> %{res}"]
  in
  List.iter [ "1"; "123345566"; "$2" ] ~f:test
;;
