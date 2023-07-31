[@@@disable_unused_warnings]

open! Core
open! Async
open! Scrape
open! Uri
open! Jsonaf.Export

(* open! postgresql *)
module BetterString = struct
  include String

  let title str =
    str
    |> String.split ~on:' '
    |> List.map ~f:(fun word -> String.capitalize word)
    |> String.concat ~sep:" "
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
  [@@deriving sexp, equal, jsonaf] [@@jsonaf.allow_extra_fields]

  let string_price t = "$" ^ t.price

  let address t =
    BetterString.title (pct_decode t.city)
    ^ ", "
    ^ (String.tr ~target:'"' ~replacement:' ' t.state |> String.strip)
  ;;

  let specs t = t.bedrooms ^ " bed, " ^ t.bathrooms ^ " bath"

  let images t =
    List.map t.images ~f:(fun url ->
      String.tr ~target:'"' ~replacement:' ' url |> String.strip)
  ;;

  let images_as_string t = String.concat ~sep:", " (images t)
  let fields = [ "zpid"; "city"; "state"; "bedrooms"; "bathrooms"; "price" ]

  let from_scraped_data ~mapped_details ~images =
    { zpid = Map.find_exn mapped_details "zpid"
    ; city = Map.find_exn mapped_details "city"
    ; state = Map.find_exn mapped_details "state"
    ; bedrooms = Map.find_exn mapped_details "bedrooms"
    ; bathrooms = Map.find_exn mapped_details "bathrooms"
    ; price = Map.find_exn mapped_details "price"
    ; images
    }
  ;;
end

let get_location_data ~location ~(houses_per_view : int)
  : string list list Deferred.t
  =
  let water_data =
    house_data ~location ~view:"water" ~n_houses:houses_per_view
  in
  let%bind () = Clock.after (Time_float.Span.of_sec 1.5) in
  (* let city_data = house_data ~location ~view:"city"
     ~n_houses:houses_per_view in *)
  let other_data = house_data ~location ~view:"" ~n_houses:houses_per_view in
  let%bind () = Clock.after (Time_float.Span.of_sec 1.5) in
  return (water_data @ other_data)
;;

let store_houses ~locations ~(houses_per_view : int) : unit Deferred.t =
  let%bind house_sexps =
    Deferred.List.concat_map locations ~how:`Sequential ~f:(fun location ->
      let%bind location_details =
        get_location_data ~location ~houses_per_view
      in
      List.map location_details ~f:(fun house_details ->
        let mapped_details =
          house_details
          |> List.zip_exn House.fields
          |> String.Map.of_alist_exn
        in
        let images : string list =
          photos ~zpid:(List.nth_exn house_details 0)
        in
        let house = House.from_scraped_data ~mapped_details ~images in
        House.sexp_of_t house)
      |> return)
  in
  Writer.save_sexps "resources/house_data.txt" house_sexps
;;

(* Scheduler.go store_houses *)

let pull_data () =
  let locations =
    [ "new york city"
    ; "los angeles"
    ; "houston"
    ; "calabasas"
    ; "chicago"
    ; "montreal"
    ; "greenwich"
    ; "bethesda"
    ; "potomac"
    ; "key west"
    ; "hollywood hills"
    ; "palo alto"
    ; "east palo alto"
    ; "miami"
    ; "seattle"
    ]
  in
  let houses_per_view = 90 / List.length locations / 2 in
  store_houses ~locations ~houses_per_view
;;

let%expect_test "house_rec" =
  let testhouse =
    House.from_scraped_data
      ~mapped_details:
        (String.Map.of_alist_exn
           [ "zpid", "123456"
           ; "city", "new york city"
           ; "state", "NY"
           ; "bedrooms", "3.0"
           ; "bathrooms", "2.0"
           ; "price", "1000000"
           ])
      ~images:[ "imageblahblahblah" ]
  in
  let houseaddy = House.address testhouse in
  print_s [%sexp (houseaddy : string)];
  [%expect {| "New York City, NY" |}];
  let housespecs = House.specs testhouse in
  print_s [%sexp (housespecs : string)];
  [%expect {| "3.0 bed, 2.0 bath" |}];
  let houseprice = House.string_price testhouse in
  print_s [%sexp (houseprice : string)];
  [%expect {| $1000000 |}];
  Deferred.return ()
;;
