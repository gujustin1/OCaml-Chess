open Chess_board
open Tile
open Command

let get_int c =
  match c with
  | '1' -> 7
  | '2' -> 6
  | '3' -> 5
  | '4' -> 4
  | '5' -> 3
  | '6' -> 2
  | '7' -> 1
  | '8' -> 0
  | _ -> failwith "impossible"

let char_to_int c =
  match c with
  | 'a' -> 0
  | 'b' -> 1
  | 'c' -> 2
  | 'd' -> 3
  | 'e' -> 4
  | 'f' -> 5
  | 'g' -> 6
  | 'h' -> 7
  | _ -> failwith "impossible"

let black_pieces tile =
  match get_piece tile with
  | Pawn -> "♙"
  | Rook -> "♖"
  | Knight -> "♘"
  | Bishop -> "♗"
  | King -> "♔"
  | Queen -> "♕"
  | Empty -> "-"

let white_pieces tile =
  match get_piece tile with
  | Pawn -> "♟︎"
  | Rook -> "♜"
  | Knight -> "♞"
  | Bishop -> "♝"
  | King -> "♚"
  | Queen -> "♛"
  | Empty -> "-"

let string_piece tile =
  if get_color tile = White then white_pieces tile
  else black_pieces tile

let rec print_row x y b : unit =
  match y with
  | 8 -> print_string "\n"
  | y ->
      print_string (string_piece b.(x).(y) ^ "  ");
      print_row x (y + 1) b

let rec print_board x b : unit =
  match x with
  | 8 -> ()
  | x ->
      print_int (8 - x);
      print_string "   ";
      print_row x 0 b;
      print_board (x + 1) b

let print b =
  print_board 0 b;
  print_string "\n    a  b  c  d  e  f  g  h\n\n"

let check_valid_move str =
  match parse str with
  | Move x -> true
  | Quit -> false
  | InvalidMove -> false

let get_lst s =
  match parse s with
  | Move o -> o
  | InvalidMove -> failwith "impossible"
  | Quit -> failwith ""

let quit s =
  match parse s with
  | Move o -> false
  | InvalidMove -> false
  | Quit -> true

let rec get_coords lst =
  match lst with
  | h :: t -> get_int h.[1] :: char_to_int h.[0] :: get_coords t
  | [] -> []

let rec print_list l =
  match l with
  | h :: t ->
      print_int (fst h);
      print_string ",";
      print_int (snd h);
      print_string "\n";
      print_list t
  | [] -> ()

let rec play_game b c =
  let o = Sys.command "clear" in
  print b;
  let _ = print_list (tester_get_king_moves b) in
  print_string "\nIf white can block: ";
  let _ = print_string (string_of_bool (tester_can_block b)) in
  print_string "\nNumber of attackers on white: ";
  let _ = print_int (tester_lst_attackers b) in
  print_string "\n";
  print_string "If white is in checkmate: ";
  let _ = print_string (string_of_bool (is_checkmate b)) in
  print_string "\n";
  let () = print_string "Enter two valid positions (e.g. a2 a4)" in
  let i = read_line () in
  if check_valid_move i then
    let lst = get_coords (get_lst i) in
    if
      check_validity b (List.nth lst 0) (List.nth lst 1)
        (List.nth lst 2) (List.nth lst 3) c
    then (
      move_piece b (List.nth lst 0) (List.nth lst 1) (List.nth lst 2)
        (List.nth lst 3);
      play_game b (c + 1))
    else (
      print_string "Invalid move, try again\n";
      if not (quit i) then play_game b c)
  else (
    print_string "Invalid move, try again\n";
    if not (quit i) then play_game b c)

let main () =
  let b = init in
  initialize b (string_to_lists starterboard) 0;
  play_game b 0

let () = main ()
