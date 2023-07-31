open! Core
open! Async_rpc_kernel

module Game_kind = struct
  type t =
    | Tic_tac_toe
    | Omok
  [@@deriving sexp, equal, bin_io]

  let to_string = Fn.compose Sexp.to_string_hum sexp_of_t

  let to_string_hum game_kind =
    game_kind
    |> sexp_of_t
    |> Sexp.to_string_hum
    |> String.lowercase
    |> String.map ~f:(function '_' -> ' ' | x -> x)
  ;;

  let board_length = function Tic_tac_toe -> 3 | Omok -> 15
  let win_length = function Tic_tac_toe -> 3 | Omok -> 5
end

module Difficulty = struct
  type t =
    | Easy
    | Medium
    | Hard
  [@@deriving sexp, equal, bin_io]

  let to_string = Fn.compose Sexp.to_string_hum sexp_of_t
end

module Game_id : sig
  type t

  include Identifiable.S with type t := t

  val of_int : int -> t
  val to_int : t -> int
end =
  Int

module Username : sig
  type t

  include Identifiable.S with type t := t
end =
  String

module With_username = struct
  type 'a t =
    { txt : 'a
    ; username : Username.t
    }
  [@@deriving sexp_of, equal, bin_io]
end

module Create_game = struct
  module Query = struct
    type t =
      { game_kind : Game_kind.t
      ; against_server_bot : Difficulty.t option
      }
    [@@deriving sexp_of, equal, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"create-game"
      ~version:0
      ~bin_query:[%bin_type_class: Query.t With_username.t]
      ~bin_response:[%bin_type_class: Game_id.t]
  ;;
end

module Join_existing_game = struct
  module Response = struct
    type t =
      | Ok
      | You've_already_joined_this_game
      | Game_does_not_exist
      | Game_already_full
      | Game_already_ended
    [@@deriving sexp_of, equal, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"join-existing-game"
      ~version:0
      ~bin_query:[%bin_type_class: Game_id.t With_username.t]
      ~bin_response:[%bin_type_class: Response.t]
  ;;
end

module Player = struct
  type t =
    | Server_bot of Difficulty.t
    | Player of Username.t
  [@@deriving sexp_of, equal, bin_io]
end

module Piece = struct
  type t =
    | X
    | O
  [@@deriving sexp, equal, bin_io, enumerate]

  let of_string = Fn.compose t_of_sexp Sexp.of_string
  let to_string = Fn.compose Sexp.to_string_hum sexp_of_t
  let flip = function X -> O | O -> X
end

module Position = struct
  module T = struct
    type t =
      { row : int
      ; column : int
      }
    [@@deriving sexp, equal, bin_io, compare]
  end

  include T
  include Comparable.Make_binable (T)

  let to_string = Fn.compose Sexp.to_string_hum sexp_of_t

  let in_bounds t ~game_kind =
    let board_length = Game_kind.board_length game_kind in
    let open Int.O in
    List.for_all [ t.row; t.column ] ~f:(fun x -> x >= 0 && x < board_length)
  ;;

  let down { row; column } = { row = row + 1; column }
  let right { row; column } = { row; column = column + 1 }
  let up { row; column } = { row = row - 1; column }
  let left { row; column } = { row; column = column - 1 }

  let all_offsets =
    let ( >> ) = Fn.compose in
    [ up
    ; up >> right
    ; right
    ; right >> down
    ; down
    ; down >> left
    ; left
    ; left >> up
    ]
  ;;
end

module Game_status = struct
  type t =
    | Turn_of of Piece.t
    | Game_over of { winner : (Piece.t * Position.Set.t) option }
  [@@deriving sexp_of, equal, bin_io]
end

module Game_state = struct
  type t =
    { game_id : Game_id.t
    ; game_kind : Game_kind.t
    ; player_x : Player.t
    ; player_o : Player.t
    ; pieces : Piece.t Position.Map.t
    ; game_status : Game_status.t
    }
  [@@deriving sexp_of, equal, bin_io]

  let get_player t ~piece =
    match piece with Piece.X -> t.player_x | O -> t.player_o
  ;;

  let to_string_hum
    { game_id; game_kind; player_x; player_o; pieces; game_status }
    =
    let board_length = Game_kind.board_length game_kind in
    let rows =
      List.init board_length ~f:(fun row ->
        List.init board_length ~f:(fun column ->
          let position = { Position.row; column } in
          match Map.find pieces position with
          | None -> " "
          | Some X -> "X"
          | Some O -> "O")
        |> String.concat)
      |> String.concat_lines
    in
    let top =
      [%message
        (game_id : Game_id.t)
          (game_kind : Game_kind.t)
          (player_x : Player.t)
          (player_o : Player.t)
          (game_status : Game_status.t)]
    in
    Sexp.to_string top ^ "\n" ^ rows
  ;;
end

module Show_all_games_with_two_players = struct
  let rpc =
    Rpc.Rpc.create
      ~name:"show-all-games-with-two-players"
      ~version:0
      ~bin_query:[%bin_type_class: unit]
      ~bin_response:[%bin_type_class: Game_state.t Game_id.Map.t]
  ;;
end

module Joinable_game = struct
  type t =
    { game_id : Game_id.t
    ; game_kind : Game_kind.t
    ; player_x : Player.t
    }
  [@@deriving sexp_of, equal, bin_io]
end

module Get_game = struct
  module Response = struct
    type t =
      | Both_players of Game_state.t
      | Waiting_for_someone_to_join of Joinable_game.t
      | Game_id_does_not_exist
    [@@deriving sexp_of, equal, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"get-game"
      ~version:0
      ~bin_query:[%bin_type_class: Game_id.t]
      ~bin_response:[%bin_type_class: Response.t]
  ;;
end

module List_all_joinable_games = struct
  let rpc =
    Rpc.Rpc.create
      ~name:"list-all-joinable-games"
      ~version:0
      ~bin_query:[%bin_type_class: unit]
      ~bin_response:[%bin_type_class: Joinable_game.t Game_id.Map.t]
  ;;
end

module Take_turn = struct
  module Query = struct
    type t =
      { game_id : Game_id.t
      ; position : Position.t
      }
    [@@deriving sexp_of, equal, bin_io]
  end

  module Response = struct
    type t =
      | Ok
      | Game_does_not_exist
      | Not_your_turn
      | Position_out_of_bounds
      | Position_already_occupied
      | Game_is_over
      | You_are_not_a_player_in_this_game
    [@@deriving sexp_of, equal, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"take-turn"
      ~version:0
      ~bin_query:[%bin_type_class: Query.t With_username.t]
      ~bin_response:[%bin_type_class: Response.t]
  ;;
end

module World_state = struct
  type t =
    { joinable_games : Joinable_game.t Game_id.Map.t
    ; running_games : Game_state.t Game_id.Map.t
    }
  [@@deriving bin_io, sexp_of, equal]

  let empty =
    { joinable_games = Game_id.Map.empty; running_games = Game_id.Map.empty }
  ;;
end

module Me = struct
  let rpc =
    Rpc.Rpc.create
      ~name:"me"
      ~version:0
      ~bin_query:[%bin_type_class: unit]
      ~bin_response:[%bin_type_class: Username.t]
  ;;
end

module Is_thinking = struct
  let rpc =
    Rpc.Rpc.create
      ~name:"is-thinking"
      ~version:0
      ~bin_query:[%bin_type_class: Game_id.t]
      ~bin_response:[%bin_type_class: bool]
  ;;
end
