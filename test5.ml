open Test
open OUnit2
open Chess_board
open Tile
open Command

let b = init

let _ = initialize b (string_to_lists starterboard) 0

let _ = move_piece b 6 3 5 3

let _ = move_piece b 1 3 2 3

let _ = move_piece b 7 2 5 4

let _ = move_piece b 0 2 2 4

let _ = move_piece b 7 3 6 3

let _ = move_piece b 0 3 1 3

let _ = move_piece b 7 1 5 2

let _ = move_piece b 0 1 2 2

let _ = move_piece b 7 4 7 2

let _ = move_piece b 0 4 0 2

let castling_tests =
  [
    get_piece_test "Pawn on d3" b 5 3 Pawn;
    get_piece_test "Pawn on d6" b 2 3 Pawn;
    get_piece_test "Bishop on e3" b 5 4 Bishop;
    get_piece_test "Bishop on e6" b 2 4 Bishop;
    get_piece_test "Queen on d2" b 6 3 Queen;
    get_piece_test "Queen on d7" b 1 3 Queen;
    get_piece_test "Knight on c3" b 5 2 Knight;
    get_piece_test "Knight on c6" b 2 2 Knight;
    get_piece_test "King on c1" b 7 2 King;
    get_piece_test "Rook on d1" b 7 3 Rook;
    get_piece_test "King on c8" b 0 2 King;
    get_piece_test "Rook on d8" b 0 3 Rook;
  ]

let suite =
  "test suite for King side castling"
  >::: List.flatten [ castling_tests ]

let _ = run_test_tt_main suite
