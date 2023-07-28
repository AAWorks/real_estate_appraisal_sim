[@@@disable_unused_warnings]

open! Core
open! Async
open! Jsonaf

module Curl = struct
  let writer accum data =
    Buffer.add_string accum data;
    String.length data
  ;;

  let get_exn url rapid =
    let error_buffer = ref "" in
    let result = Buffer.create 16384 in
    let fail error = failwithf "Curl failed on %s: %s" url error () in
    try
      let connection = Curl.init () in
      Curl.set_errorbuffer connection error_buffer;
      Curl.set_writefunction connection (writer result);
      Curl.set_followlocation connection true;
      Curl.set_url connection url;
      let test = Curl.get_effectiveurl connection in
      Curl.set_httpheader connection rapid;
      Curl.perform connection;
      let result = Buffer.contents result in
      Curl.cleanup connection;
      result
    with
    | Curl.CurlException (_reason, _code, _str) -> fail !error_buffer
    | Failure s -> fail s
  ;;
end

let house_data ~(location : string) ~(view : string) ~(n_houses : int) =
  let location =
    String.substr_replace_all location ~pattern:" " ~with_:"%20"
  in
  let view_link =
    match view with
    | "water" -> "isWaterfront=true"
    | "city" -> "isCityView=true"
    | _ -> ""
  in
  let link =
    "https://zillow56.p.rapidapi.com/search?location="
    ^ location
    ^ "&status=forSale&isSingleFamily=true&isMultiFamily=false&isApartment=false&isCondo=false&isManufactured=false&isTownhouse=false&isLotLand=false&price_min=300000&"
    ^ view_link
    ^ "&singleStory=false&onlyWithPhotos=true"
  in
  let rapid =
    [ "X-RapidAPI-Key: c41557561amsha79320cdb4ab359p156dfcjsn61eb3e8d818d"
    ; "X-RapidAPI-Host: zillow56.p.rapidapi.com"
    ]
  in
  let json = Jsonaf.of_string (Curl.get_exn link rapid) in
  let results = json |> member_exn "results" |> list_exn in
  let results, _ = List.split_n results n_houses in
  List.map results ~f:(fun house ->
    let bathroom = house |> member_exn "bathrooms" |> to_string_hum in
    let zpid = house |> member_exn "zpid" |> to_string_hum in
    let state = house |> member_exn "state" |> to_string_hum in
    let bedroom = house |> member_exn "bedrooms" |> to_string_hum in
    let price = house |> member_exn "price" |> to_string_hum in
    [ zpid; location; state; bedroom; bathroom; price ])
;;

let%expect_test "house_call" =
  let house_data = house_data ~location:"houston" ~view:"" ~n_houses:1 in
  print_s [%message "" (house_data : string list list)];
  [%expect {| [2066641016, Houston, TX, 1, 1, 125000]|}];
  Deferred.return ()
;;

let photos ~(zpid : string) =
  let link =
    "https://zillow-data-v2.p.rapidapi.com/properties/detail?zpid=" ^ zpid
  in
  let rapid =
    [ "X-RapidAPI-Key: c41557561amsha79320cdb4ab359p156dfcjsn61eb3e8d818d"
    ; "X-RapidAPI-Host: zillow-data-v2.p.rapidapi.com"
    ]
  in
  let file = Curl.get_exn link rapid in
  let json = Jsonaf.of_string file in
  let photos =
    json |> member_exn "data" |> member_exn "responsivePhotos" |> list_exn
  in
  List.map photos ~f:(fun key ->
    let webp =
      key |> member_exn "mixedSources" |> member_exn "webp" |> list_exn
    in
    let chosen = List.nth_exn webp 3 in
    chosen |> member_exn "url" |> to_string_hum)
;;
