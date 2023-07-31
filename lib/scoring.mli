open! Core

val is_price : ?allowed_error:int -> int -> int -> bool

val weighted_points
  :  actual:int
  -> guess:int
  -> ?point_scale:float
  -> unit
  -> int
