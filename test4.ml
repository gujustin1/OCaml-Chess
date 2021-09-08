open Test
open OUnit2
open Chess_board
open Tile
open Command

let b = init

let _ = initialize b (string_to_lists starterboard) 0

let _ = move_piece b 6 4 5 4

let _ = move_piece b 1 4 2 4

let _ = move_piece b 7 5 6 4

let _ = move_piece b 1 5 2 5

let _ = move_piece b 7 6 5 5

let _ = move_piece b 1 7 2 7

let _ = move_piece b 7 4 7 6

let castling_tests =
  [
    get_piece_test "Pawn on e3" b 5 4 Pawn;
    get_piece_test "Pawn on e6" b 2 4 Pawn;
    get_piece_test "Bishop on e2" b 6 4 Bishop;
    get_piece_test "Pawn on f6" b 2 5 Pawn;
    get_piece_test "Knight on f3" b 5 5 Knight;
    get_piece_test "Pawn on g6" b 2 7 Pawn;
    get_piece_test "King on g1" b 7 6 King;
    get_piece_test "Rook on f1" b 7 5 Rook;
  ]

let suite =
  "test suite for King side castling"
  >::: List.flatten [ castling_tests ]

let _ = run_test_tt_main suite
