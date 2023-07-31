open! Core
open! Async
open! Storage
open! Questionbank

let prompt_for_guess ~show_hint ~house =
  let rec prompt_for_price price_input =
    Core.Printf.printf "%s " price_input;
    let guess =
      Out_channel.(flush stdout);
      Int.of_string In_channel.(input_line_exn stdin)
    in
    if guess <= 0
    then (
      Core.Printf.printf "Price must be greater than 0.\n";
      prompt_for_price price_input)
    else guess
  in
  Core.Printf.printf "House photos: %s\n" (List.hd_exn (House.images house));
  Core.Printf.printf "Location: %s\n" (House.address house);
  Core.Printf.printf "Specs: %s\n\n" (House.specs house);
  (* Core.Printf.printf "***DIAG: %s***\n" (House.string_price house); *)
  Core.Printf.printf (if show_hint then "Take a guess\n" else "");
  let x = prompt_for_price ">" in
  x
;;

(* if x = then ( Printf.printf "%d is too basic. Try again.\n" x;
   prompt_for_move ~show_hint:true) else x *)

let rec run_game ~questions ~points : unit =
  match questions with
  | house :: remaining ->
    let guess = prompt_for_guess ~show_hint:true ~house in
    let new_pts =
      Scoring.weighted_points ~actual:(House.int_price house) ~guess ()
    in
    Core.Printf.printf
      "Your Guess: %s\n\
       Actual Price: %s\n\
       Points Earned This Round: %d\n\
       Total Points: %d\n\n"
      (BetterString.to_price_string guess)
      (House.string_price house)
      new_pts
      (points + new_pts);
    run_game ~questions:remaining ~points:(new_pts + points)
  | [] -> Core.Printf.printf "Thanks for playing! Total Score: %d\n\n" points
;;

let run () =
  let%bind questions = questions_as_records () in
  Core.Printf.printf "Welcome to Property Prodigy!\n\n";
  Deferred.return (run_game ~questions ~points:0)
;;
