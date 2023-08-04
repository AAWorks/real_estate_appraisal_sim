open! Core

(** The [Page] module represents the structure of your sites URL and is ursed
    for URL routing for your site. *)

type t =
  | Homepage
  | Game of int
  | Leaderboard 
[@@deriving sexp, equal]

val parser : t Uri_parsing.Versioned_parser.t
