open Test
open OUnit2
open Chess_board
open Tile
open Command

let b = init

let _ = initialize b (string_to_lists starterboard) 0

let tile_tests =
  [
    get_piece_test "Rook Check" b 0 0 Rook;
    get_piece_test "Knight Check" b 0 1 Knight;
    get_piece_test "Bishop Check" b 0 2 Bishop;
    get_piece_test "Queen Check" b 0 3 Queen;
    get_piece_test "King Check" b 7 4 King;
    get_piece_test "Rook Check" b 3 0 Empty;
    get_color_test "White Piece Color Check" b 0 5 Black;
    get_color_test "Black Piece Color Check" b 6 6 White;
    get_color_test "No Piece Color Check" b 3 0 None;
    point_value_test "Rook Value" b 0 0 5;
    point_value_test "Empty Value" b 3 0 0;
  ]

let suite =
  "test suite for starterboard" >::: List.flatten [ tile_tests ]

let _ = run_test_tt_main suite
