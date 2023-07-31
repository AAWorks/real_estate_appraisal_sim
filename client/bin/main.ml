open! Core
open! Async_kernel
open Tic_tac_toe_2023_lib
module Url_var = Bonsai_web_ui_url_var

let url_var =
  Bonsai_web_ui_url_var.Typed.make
    (module Page)
    Page.parser
    ~encoding_behavior:Correct
    ~fallback:(fun _ _ -> Homepage)
;;

let run () =
  Async_js.init ();
  let url = Url_var.value url_var in
  let set_url = Url_var.set_effect url_var in
  let () = Bonsai_web.Start.start (App.component ~url ~set_url) in
  Deferred.never ()
;;

let () = don't_wait_for (run ())
