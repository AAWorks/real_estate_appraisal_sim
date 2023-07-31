open! Core
open! Bonsai_web_test
open! Bonsai_web
open Tic_tac_toe_2023_lib
open Async_kernel
open Async_js_test

let%expect_test _ =
  Uri_parsing.Versioned_parser.check_ok_and_print_urls_or_errors Page.parser;
  [%expect
    {|
    URL parser looks good!
    ┌────────────────┐
    │ All urls       │
    ├────────────────┤
    │ /              │
    │ /game/<string> │
    └────────────────┘ |}];
  return ()
;;
