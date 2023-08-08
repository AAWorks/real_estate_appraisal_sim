open! Core
open! Async
open! Real_estate_appraisal_sim

let command =
  Command.async
    ~summary:"summary"
    (let%map_open.Command () = return () in
     fun () -> Storage.pull_data ())
;;

let () = Command_unix.run command
