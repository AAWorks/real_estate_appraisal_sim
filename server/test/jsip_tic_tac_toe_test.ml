open! Core
open! Tic_tac_toe_2023_native.For_testing
open! Tic_tac_toe_2023_common
open! Protocol

let print_state state = print_endline (State.to_string_hum state)

let%expect_test "[create]" =
  let state = State.create ~world_state:World_state.empty in
  print_state state;
  [%expect {|
    next_id: 0
    (joinable_games()) |}]
;;

let first_player = Username.of_string "charmander"
let second_player = Username.of_string "bulbasaur"
let third_player = Username.of_string "snorlax"

let%expect_test "two [create]'s" =
  let state = State.create ~world_state:World_state.empty in
  print_state state;
  [%expect {|
    next_id: 0
    (joinable_games()) |}];
  let (_ : Game_id.t) =
    State.create_game
      state
      ~first_player
      ~query:{ against_server_bot = None; game_kind = Omok }
  in
  let (_ : Game_id.t) =
    State.create_game
      state
      ~first_player
      ~query:{ against_server_bot = None; game_kind = Omok }
  in
  print_state state;
  [%expect
    {|
    next_id: 2
    (joinable_games((1((game_id 1)(game_kind Omok)(player_x(Player charmander))))(2((game_id 2)(game_kind Omok)(player_x(Player charmander)))))) |}]
;;

let%expect_test "[create]'ing and [join]'ing a game" =
  let state = State.create ~world_state:World_state.empty in
  let game_id =
    State.create_game
      state
      ~first_player
      ~query:
        { Create_game.Query.against_server_bot = None
        ; game_kind = Tic_tac_toe
        }
  in
  print_state state;
  [%expect
    {|
    next_id: 1
    (joinable_games((1((game_id 1)(game_kind Tic_tac_toe)(player_x(Player charmander)))))) |}];
  let response = State.join_game state ~second_player ~game_id in
  print_s [%message (response : Join_existing_game.Response.t)];
  [%expect {| (response Ok) |}];
  print_state state;
  [%expect
    {|
    next_id: 1
    (joinable_games())
    ((game_id 1)(game_kind Tic_tac_toe)(player_x(Player charmander))(player_o(Player bulbasaur))(game_status(Turn_of X))) |}]
;;

let%expect_test "Attempting to join an already existing game" =
  let state = State.create ~world_state:World_state.empty in
  let game_id =
    State.create_game
      state
      ~first_player:(Username.of_string "charmander")
      ~query:
        { Create_game.Query.against_server_bot = None
        ; game_kind = Tic_tac_toe
        }
  in
  let _response =
    State.join_game
      state
      ~second_player:(Username.of_string "bulbasaur")
      ~game_id
  in
  print_state state;
  [%expect
    {|
    next_id: 1
    (joinable_games())
    ((game_id 1)(game_kind Tic_tac_toe)(player_x(Player charmander))(player_o(Player bulbasaur))(game_status(Turn_of X))) |}];
  let response =
    State.join_game state ~second_player:third_player ~game_id
  in
  print_s [%message (response : Join_existing_game.Response.t)];
  [%expect {| (response Game_already_full) |}]
;;

let%expect_test "Attempting to join a game that does not exist" =
  let state = State.create ~world_state:World_state.empty in
  let game_id = Game_id.of_string "2" in
  let response =
    State.join_game state ~second_player:third_player ~game_id
  in
  print_s [%message (response : Join_existing_game.Response.t)];
  [%expect {| (response Game_does_not_exist) |}]
;;

let%expect_test "[create]'ing a game against an AI results in the game \
                 being immediately joined"
  =
  let state = State.create ~world_state:World_state.empty in
  let _game_id =
    State.create_game
      state
      ~first_player
      ~query:
        { Create_game.Query.against_server_bot = Some Easy
        ; game_kind = Tic_tac_toe
        }
  in
  print_state state;
  [%expect
    {|
    next_id: 1
    (joinable_games())
    ((game_id 1)(game_kind Tic_tac_toe)(player_x(Player charmander))(player_o(Server_bot Easy))(game_status(Turn_of X))) |}]
;;

let%test_module "taking turns" =
  (module struct
    open Async
    open Deferred.Let_syntax

    let create_joined_game () =
      let state = State.create ~world_state:World_state.empty in
      let game_id =
        State.create_game
          state
          ~first_player
          ~query:
            { Create_game.Query.against_server_bot = None
            ; game_kind = Tic_tac_toe
            }
      in
      let _response = State.join_game state ~second_player ~game_id in
      game_id, state
    ;;

    let%expect_test "[Not_your_turn]" =
      let game_id, state = create_joined_game () in
      let%map response =
        State.take_turn
          state
          ~game_id
          ~position:{ row = 0; column = 0 }
          ~username:second_player
      in
      print_s [%message (response : Take_turn.Response.t)];
      [%expect {| (response Not_your_turn) |}]
    ;;

    let%expect_test "[Not_in_game]" =
      let game_id, state = create_joined_game () in
      let%bind response =
        State.take_turn
          state
          ~game_id
          ~position:{ row = 0; column = 0 }
          ~username:third_player
      in
      print_s [%message (response : Take_turn.Response.t)];
      [%expect {| (response You_are_not_a_player_in_this_game) |}];
      return ()
    ;;

    let%expect_test "[Out_of_bounds]" =
      let game_id, state = create_joined_game () in
      let%bind response =
        State.take_turn
          state
          ~game_id
          ~position:{ row = 100; column = 100 }
          ~username:first_player
      in
      print_s [%message (response : Take_turn.Response.t)];
      [%expect {| (response Position_out_of_bounds) |}];
      return ()
    ;;

    let%expect_test "successfully placing piece" =
      let game_id, state = create_joined_game () in
      let%bind response =
        State.take_turn
          state
          ~game_id
          ~position:{ row = 0; column = 0 }
          ~username:first_player
      in
      print_s [%message (response : Take_turn.Response.t)];
      [%expect {| (response Ok) |}];
      print_state state;
      [%expect
        {|
        next_id: 1
        (joinable_games())
        ((game_id 1)(game_kind Tic_tac_toe)(player_x(Player charmander))(player_o(Player bulbasaur))(game_status(Turn_of O)))
        X |}];
      return ()
    ;;

    let%expect_test "Attempting to place a piece on an already plaed piece" =
      let game_id, state = create_joined_game () in
      let%bind _response =
        State.take_turn
          state
          ~game_id
          ~position:{ row = 0; column = 0 }
          ~username:first_player
      in
      let%bind response =
        State.take_turn
          state
          ~game_id
          ~position:{ row = 0; column = 0 }
          ~username:second_player
      in
      print_s [%message (response : Take_turn.Response.t)];
      [%expect {| (response Position_already_occupied) |}];
      return ()
    ;;

    let%expect_test "Turn gets flipped" =
      let game_id, state = create_joined_game () in
      let%bind response =
        State.take_turn
          state
          ~game_id
          ~position:{ row = 0; column = 0 }
          ~username:first_player
      in
      print_s [%message (response : Take_turn.Response.t)];
      [%expect {| (response Ok) |}];
      let%bind response =
        State.take_turn
          state
          ~game_id
          ~position:{ row = 1; column = 0 }
          ~username:first_player
      in
      (* It is no longer player 1's turn. *)
      print_s [%message (response : Take_turn.Response.t)];
      [%expect {| (response Not_your_turn) |}];
      return ()
    ;;

    let test_sequence_of_turns state ~game_id ~moves =
      Deferred.List.iteri
        ~how:`Sequential
        moves
        ~f:(fun i (player, row, column) ->
        let%bind response =
          State.take_turn
            state
            ~game_id
            ~position:{ row; column }
            ~username:player
        in
        print_s [%message (i : int) (response : Take_turn.Response.t)];
        return ())
    ;;

    let%expect_test "Player 1 wins (horizontal)" =
      let game_id, state = create_joined_game () in
      let moves =
        [ first_player, 0, 0
        ; second_player, 2, 0
        ; first_player, 0, 1
        ; second_player, 2, 1
        ; first_player, 0, 2
        ; second_player, 2, 2
        ]
      in
      let%bind () = test_sequence_of_turns ~moves ~game_id state in
      print_state state;
      [%expect
        {|
        ((i 0) (response Ok))
        ((i 1) (response Ok))
        ((i 2) (response Ok))
        ((i 3) (response Ok))
        ((i 4) (response Ok))
        ((i 5) (response Game_is_over))
        next_id: 1
        (joinable_games())
        ((game_id 1)(game_kind Tic_tac_toe)(player_x(Player charmander))(player_o(Player bulbasaur))(game_status(Game_over(winner((X(((row 0)(column 0))((row 0)(column 1))((row 0)(column 2)))))))))
        XXX

        OO |}];
      return ()
    ;;

    let%expect_test "Player 2 wins (vertical)" =
      let game_id, state = create_joined_game () in
      let moves =
        [ first_player, 0, 0
        ; second_player, 2, 2
        ; first_player, 1, 1
        ; second_player, 2, 1
        ; first_player, 0, 2
        ; second_player, 2, 0
        ]
      in
      let%bind () = test_sequence_of_turns ~moves ~game_id state in
      print_state state;
      [%expect
        {|
        ((i 0) (response Ok))
        ((i 1) (response Ok))
        ((i 2) (response Ok))
        ((i 3) (response Ok))
        ((i 4) (response Ok))
        ((i 5) (response Ok))
        next_id: 1
        (joinable_games())
        ((game_id 1)(game_kind Tic_tac_toe)(player_x(Player charmander))(player_o(Player bulbasaur))(game_status(Game_over(winner((O(((row 2)(column 0))((row 2)(column 1))((row 2)(column 2)))))))))
        X X
         X
        OOO |}];
      return ()
    ;;

    let%expect_test "Player 1 wins (main diagonal)" =
      let game_id, state = create_joined_game () in
      let moves =
        [ first_player, 0, 0
        ; second_player, 2, 0
        ; first_player, 1, 1
        ; second_player, 2, 1
        ; first_player, 2, 2
        ]
      in
      let%bind () = test_sequence_of_turns ~moves ~game_id state in
      print_state state;
      [%expect
        {|
        ((i 0) (response Ok))
        ((i 1) (response Ok))
        ((i 2) (response Ok))
        ((i 3) (response Ok))
        ((i 4) (response Ok))
        next_id: 1
        (joinable_games())
        ((game_id 1)(game_kind Tic_tac_toe)(player_x(Player charmander))(player_o(Player bulbasaur))(game_status(Game_over(winner((X(((row 0)(column 0))((row 1)(column 1))((row 2)(column 2)))))))))
        X
         X
        OOX |}];
      return ()
    ;;

    let%expect_test "Player 1 wins (secondary diagonal)" =
      let game_id, state = create_joined_game () in
      let moves =
        [ first_player, 0, 0
        ; second_player, 0, 2
        ; first_player, 2, 1
        ; second_player, 1, 1
        ; first_player, 2, 2
        ; second_player, 2, 0
        ]
      in
      let%bind () = test_sequence_of_turns ~moves ~game_id state in
      print_state state;
      [%expect
        {|
        ((i 0) (response Ok))
        ((i 1) (response Ok))
        ((i 2) (response Ok))
        ((i 3) (response Ok))
        ((i 4) (response Ok))
        ((i 5) (response Ok))
        next_id: 1
        (joinable_games())
        ((game_id 1)(game_kind Tic_tac_toe)(player_x(Player charmander))(player_o(Player bulbasaur))(game_status(Game_over(winner((O(((row 0)(column 2))((row 1)(column 1))((row 2)(column 0)))))))))
        X O
         O
        OXX |}];
      return ()
    ;;

    let%expect_test "Game results in a tie." =
      let game_id, state = create_joined_game () in
      let moves =
        [ first_player, 0, 0
        ; second_player, 0, 1
        ; first_player, 1, 1
        ; second_player, 0, 2
        ; first_player, 1, 2
        ; second_player, 1, 0
        ; first_player, 2, 0
        ; second_player, 2, 2
        ; first_player, 2, 1
        ]
      in
      let%bind () = test_sequence_of_turns ~moves ~game_id state in
      print_state state;
      [%expect
        {|
        ((i 0) (response Ok))
        ((i 1) (response Ok))
        ((i 2) (response Ok))
        ((i 3) (response Ok))
        ((i 4) (response Ok))
        ((i 5) (response Ok))
        ((i 6) (response Ok))
        ((i 7) (response Ok))
        ((i 8) (response Ok))
        next_id: 1
        (joinable_games())
        ((game_id 1)(game_kind Tic_tac_toe)(player_x(Player charmander))(player_o(Player bulbasaur))(game_status(Game_over(winner()))))
        XOO
        OXX
        XXO |}];
      return ()
    ;;
  end)
;;

let%expect_test "Taking turns against an AI results in the AI making moves." =
  let state = State.create ~world_state:World_state.empty in
  let game_id =
    State.create_game
      state
      ~first_player
      ~query:
        { Create_game.Query.against_server_bot = Some Easy
        ; game_kind = Tic_tac_toe
        }
  in
  let _response =
    State.take_turn
      state
      ~game_id
      ~position:{ row = 0; column = 0 }
      ~username:first_player
  in
  print_state state;
  [%expect
    {|
    next_id: 1
    (joinable_games())
    ((game_id 1)(game_kind Tic_tac_toe)(player_x(Player charmander))(player_o(Server_bot Easy))(game_status(Turn_of O)))
    X |}]
;;

let%expect_test "Taking turns against an AI results in the AI making moves. \
                 - medium"
  =
  let state = State.create ~world_state:World_state.empty in
  let game_id =
    State.create_game
      state
      ~first_player
      ~query:
        { Create_game.Query.against_server_bot = Some Hard
        ; game_kind = Tic_tac_toe
        }
  in
  let _response =
    State.take_turn
      state
      ~game_id
      ~position:{ row = 0; column = 0 }
      ~username:first_player
  in
  print_state state;
  [%expect
    {|
    next_id: 1
    (joinable_games())
    ((game_id 1)(game_kind Tic_tac_toe)(player_x(Player charmander))(player_o(Server_bot Hard))(game_status(Turn_of O)))
    X |}]
;;
