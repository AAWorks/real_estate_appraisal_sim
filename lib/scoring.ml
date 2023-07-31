open! Core

let is_price ?(allowed_error = 1 / 10) actual guess =
  let margin = actual * allowed_error in
  actual - margin < guess && guess < actual + margin
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
