[@@@disable_unused_warnings]

open! Core
open! Async
open! Jsonaf.Export
(* open! postgresql *)

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
  [@@deriving sexp, equal] [@@jsonaf.allow_extra_fields]

  let string_price t = "$" ^ t.price
  let address t = t.city ^ ", " ^ t.state
  let specs t = t.bedrooms ^ " bed, " ^ t.bathrooms ^ " bath"

  let fields =
    [ "zpid"; "city"; "state"; "bedrooms"; "bathrooms"; "price"; "images" ]
  ;;

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

let house_data ~location:_ ~view:_ ~n_houses:_ = assert false
let photos ~zpid:_ = assert false

let get_location_data ~location ~(houses_per_view : int) : string list list =
  let water_data =
    house_data ~location ~view:"water" ~n_houses:houses_per_view
  in
  let city_data =
    house_data ~location ~view:"city" ~n_houses:houses_per_view
  in
  water_data @ city_data
;;

let store_houses ~locations ~(houses_per_view : int) : unit Deferred.t =
  let house_sexps =
    List.concat_map locations ~f:(fun location ->
      let location_details = get_location_data ~location ~houses_per_view in
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
        House.sexp_of_t house))
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
    ; "martha%Cs vineyard"
    ; "hollywood hills"
    ; "palo alto"
    ; "east palo alto"
    ; "miami"
    ; "seattle"
    ]
  in
  let houses_per_view = 180 / List.length locations / 2 in
  store_houses ~locations ~houses_per_view
;;

let%expect_test "house_rec" =
  let testhouse =
    House.from_scraped_data
      ~mapped_details:
        (String.Map.of_alist_exn
           [ "zpid", "123456"
           ; "city", "NYC"
           ; "state", "NY"
           ; "bedrooms", "3.0"
           ; "bathrooms", "2.0"
           ; "price", "1000000"
           ])
      ~images:[ "imageblahblahblah" ]
  in
  let houseaddy = House.address testhouse in
  print_s [%sexp (houseaddy : string)];
  [%expect {| "NYC, NY" |}];
  let housespecs = House.specs testhouse in
  print_s [%sexp (housespecs : string)];
  [%expect {| "3.0 bed, 2.0 bath" |}];
  let houseprice = House.string_price testhouse in
  print_s [%sexp (houseprice : string)];
  [%expect {| $1000000 |}];
  Deferred.return ()
;;
