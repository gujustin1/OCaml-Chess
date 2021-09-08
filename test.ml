(** Test plan: There were some problems with testing mutable matrixes.
    If 2 chessboards were initialized, they would always have the same
    values as each other. Thus, for each board and scenario, there are
    seperate test files. test.ml contains all the testing functions.
    test1.ml tests the starting board of a chess game by checking the
    tile on a specific coordinate. test2.ml tests the possible moves of
    several pieces which includes potential capture moves by pawns,
    knights, etc.. test3.ml tests a checkmate position as well as the
    move_piece function. And test4.ml and test 5.ml test King-side and
    Queen-side castling respectively. There are too many chess
    possibilites to do glass box testing, thus we did black box testing.
    In addition to testing key features, we did a lot more manual
    testing. For example, make play is a console implementation of Chess
    that has some debugging features such as showing whether white is in
    checkmate, if white can block to avoid checkmate, and the number of
    attacks on white. With all the key features of chess tested
    (checkmate, castling, moving pieces, capturing), the system is
    correct.*)

open OUnit2
open Chess_board
open Tile
open Command

let test_board =
  "r,n,b,k,q,b,n,r/p, ,p,p, , , , / ,p, , ,p,p, , / , , ,R, , ,p, / , \
   , , , , , , / , ,P,P, ,N, ,p/P,P, , ,P,P,P,P/ ,N,B,K,Q,B, ,R"

let checkmatepos =
  "r,n,b,k, ,b,n,r/p,p,p,p,p, ,p,p/ , , , , , , , / , , , , ,p, ,q/ , \
   , , ,P, , , / , , ,P, , , , /P,P,P, , ,P,P,P/R,N,B,K,Q,B,N,R"

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

let piece_to_string p =
  match p with
  | Pawn -> "♙"
  | Rook -> "♖"
  | Knight -> "♘"
  | Bishop -> "♗"
  | King -> "♔"
  | Queen -> "♕"
  | Empty -> "-"

let get_piece_test name bd row col expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_piece bd.(row).(col))
    ~printer:piece_to_string

let get_color_test name b row col expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (get_color b.(row).(col))

let point_value_test name b row col expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (point_value b.(row).(col))

let posib_moves_test name b row col expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (possible_moves row col b)
    ~cmp:cmp_set_like_lists

let white_score_test name b x y x2 y2 expected_output : test =
  name >:: fun _ ->
  let filler = check_validity b x y x2 y2 0 in
  move_piece b x y x2 y2;
  assert_equal expected_output get_white_score

let black_score_test name expected_output : test =
  name >:: fun _ -> assert_equal expected_output get_black_score

let check_tiles_test name row col row2 col2 b expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    [ get_piece b.(row).(col); get_piece b.(row2).(col2) ]

let check_test name expected_output : test =
  name >:: fun _ -> assert_equal expected_output (in_check Tile.White)

let checkmate_test name b expected_output : test =
  name >:: fun _ -> assert_equal expected_output (is_checkmate b)
