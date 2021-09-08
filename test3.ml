open Test
open OUnit2
open Chess_board
open Tile
open Command

let b = init

let _ = initialize b (string_to_lists starterboard) 0

let _ = move_piece b 6 5 5 5

let _ = move_piece b 1 4 2 4

let _ = move_piece b 6 6 4 6

let _ = move_piece b 0 3 4 7

let checkmate_tests =
  [
    get_piece_test "Pawn on f3" b 5 5 Pawn;
    get_piece_test "Pawn on e6" b 2 4 Pawn;
    get_piece_test "Pawn on g4" b 4 6 Pawn;
    get_piece_test "Queen on h4" b 4 7 Queen;
    get_piece_test "King on e1" b 7 4 King;
    checkmate_test "Checkmate" b true;
  ]

let suite =
  "test suite for Checkmate" >::: List.flatten [ checkmate_tests ]

let _ = run_test_tt_main suite
