open! Core
open! Async
open! Storage
open! Questionbank

let prompt_for_guess ~show_hint ~house =
  let rec prompt_for_price price =
    Core.Printf.printf "%s " price;
    let guess =
      Out_channel.(flush stdout);
      Int.of_string In_channel.(input_line_exn stdin)
    in
    if guess <= 0
    then (
      Core.Printf.printf "Price must be greater than 0.\n";
      prompt_for_price price)
    else guess
  in
  Core.Printf.printf "House photos: %s\n" (List.hd_exn (House.images house));
  Core.Printf.printf "Location: %s\n" (House.address house);
  Core.Printf.printf "Specs: %s\n\n" (House.specs house);
  Core.Printf.printf (if show_hint then "Take a guess\n" else "");
  let x = prompt_for_price ">" in
  x
;;

(* if x = then ( Printf.printf "%d is too basic. Try again.\n" x;
   prompt_for_move ~show_hint:true) else x *)

let rec run_game ~questions : unit =
  match questions with
  | head :: tail ->
    let guess = prompt_for_guess ~show_hint:true ~house:head in
    Core.Printf.printf
      "You guessed: $%d\nActual price: $%d\n\n"
      guess
      (guess + 1);
    run_game ~questions:tail
  | [] -> Core.Printf.printf "Thanks for playing!"
;;

let run () =
  let%bind questions = questions_as_records () in
  Core.Printf.printf "Welcome to Property Prodigy\n\n";
  Deferred.return (run_game ~questions)
;;
