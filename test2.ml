open Test
open OUnit2
open Chess_board
open Tile
open Command

let b = init

let _ = initialize b (string_to_lists test_board) 0

let posib_moves_tests =
  [
    posib_moves_test "Black Rook Blocked in Corner" b 0 0 [];
    posib_moves_test "White Rook in Middle of Board" b 3 3
      [
        (3, 0);
        (3, 1);
        (3, 2);
        (3, 4);
        (3, 5);
        (3, 6);
        (4, 3);
        (2, 3);
        (1, 3);
      ];
    posib_moves_test "White Queen with Blocking Pawn" b 7 4 [ (6, 3) ];
    posib_moves_test "Black Queen with Diagonal/Vert" b 0 4
      [ (1, 4); (1, 5); (2, 6); (3, 7) ];
    posib_moves_test "White King Moves" b 7 3 [ (6, 2); (6, 3) ];
    posib_moves_test "White Knight in the Middle of Board" b 5 5
      [ (4, 7); (3, 6); (4, 3); (3, 4); (7, 6); (6, 3) ];
    posib_moves_test "White Bishop Attacking Black Pawn" b 7 2
      [ (6, 3); (5, 4); (4, 5); (3, 6) ];
    posib_moves_test "(1,3) Black Pawn initial move with Blocker" b 1 3
      [ (2, 3) ];
    posib_moves_test "(2,4) Black Pawn with Capture" b 2 4
      [ (3, 3); (3, 4) ];
    posib_moves_test "(6,6) White Pawn Initial State with Capture" b 6 6
      [ (5, 6); (4, 6); (5, 7) ];
    posib_moves_test "(5,7) White Pawn Blocked off" b 5 7 [ (6, 6) ];
    posib_moves_test "(2,1) Normal Black Pawn Move" b 2 1 [ (3, 1) ];
    check_tiles_test "6 0 and 5 0 Tiles with pawn up" 6 0 5 0
      (let _ = move_piece b 6 0 5 0 in
       b)
      [ Empty; Pawn ];
  ]

let suite =
  "test suite for Chess" >::: List.flatten [ posib_moves_tests ]

let _ = run_test_tt_main suite
