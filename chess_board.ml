open Tile

exception InvalidMove

type board = Tile.tile array array

type c =
  | None
  | White
  | Black

type scoreboard = {
  mutable white_score : int;
  mutable black_score : int;
  mutable white_captured : Tile.p list;
  mutable black_captured : Tile.p list;
}

type kings = {
  mutable white_king : int * int;
  mutable black_king : int * int;
  mutable white_king_moved : bool;
  mutable black_king_moved : bool;
}

type queens = {
  mutable white_queen : int * int;
  mutable black_queen : int * int;
}

let q = { white_queen = (7, 3); black_queen = (0, 3) }

type attacked_positions = {
  mutable white : (int * int) list;
  mutable black : (int * int) list;
}

let k =
  {
    white_king = (7, 4);
    black_king = (0, 4);
    white_king_moved = false;
    black_king_moved = false;
  }

type rook = {
  mutable white_left_rook : int * int;
  mutable white_right_rook : int * int;
  mutable black_left_rook : int * int;
  mutable black_right_rook : int * int;
  mutable white_left_rook_moved : bool;
  mutable white_right_rook_moved : bool;
  mutable black_left_rook_moved : bool;
  mutable black_right_rook_moved : bool;
}

let r =
  {
    white_left_rook = (7, 0);
    white_right_rook = (7, 7);
    black_left_rook = (0, 0);
    black_right_rook = (0, 7);
    white_left_rook_moved = false;
    white_right_rook_moved = false;
    black_left_rook_moved = false;
    black_right_rook_moved = false;
  }

let attacking_pos = { white = []; black = [] }

let s =
  {
    white_score = 0;
    black_score = 0;
    white_captured = [];
    black_captured = [];
  }

(* The next 4 functions are helpers for checking the vertical and
   horizontal possible moves.*)
let rec check_left_horz row col acc init_color board =
  if col > 0 then
    if get_piece board.(row).(col - 1) = Empty then
      check_left_horz row (col - 1) ((row, col) :: acc) init_color board
    else if get_color board.(row).(col - 1) <> init_color then
      (row, col) :: (row, col - 1) :: acc
    else (row, col) :: acc
  else (row, col) :: acc

let rec check_right_horz row col acc init_color board =
  if col < 7 then
    if get_piece board.(row).(col + 1) = Empty then
      check_right_horz row (col + 1) ((row, col) :: acc) init_color
        board
    else if get_color board.(row).(col + 1) <> init_color then
      (row, col) :: (row, col + 1) :: acc
    else (row, col) :: acc
  else (row, col) :: acc

let rec check_up_vert row col acc init_color board =
  if row > 0 then
    if get_piece board.(row - 1).(col) = Empty then
      check_up_vert (row - 1) col ((row, col) :: acc) init_color board
    else if get_color board.(row - 1).(col) <> init_color then
      (row, col) :: (row - 1, col) :: acc
    else (row, col) :: acc
  else (row, col) :: acc

let rec check_down_vert row col acc init_color board =
  if row < 7 then
    if get_piece board.(row + 1).(col) = Empty then
      check_down_vert (row + 1) col ((row, col) :: acc) init_color board
    else if get_color board.(row + 1).(col) <> init_color then
      (row, col) :: (row + 1, col) :: acc
    else (row, col) :: acc
  else (row, col) :: acc

(* The next 4 functions are helpers for checking the diagonal possible
   moves.*)
let rec diagonal_up_left row col acc init_color board =
  if row > 0 && col > 0 then
    if get_piece board.(row - 1).(col - 1) = Empty then
      diagonal_up_left (row - 1) (col - 1) ((row, col) :: acc)
        init_color board
    else if get_color board.(row - 1).(col - 1) <> init_color then
      (row, col) :: (row - 1, col - 1) :: acc
    else (row, col) :: acc
  else (row, col) :: acc

let rec diagonal_up_right row col acc init_color board =
  if row > 0 && col < 7 then
    if get_piece board.(row - 1).(col + 1) = Empty then
      diagonal_up_right (row - 1) (col + 1) ((row, col) :: acc)
        init_color board
    else if get_color board.(row - 1).(col + 1) <> init_color then
      (row, col) :: (row - 1, col + 1) :: acc
    else (row, col) :: acc
  else (row, col) :: acc

let rec diagonal_down_right row col acc init_color board =
  if row < 7 && col < 7 then
    if get_piece board.(row + 1).(col + 1) = Empty then
      diagonal_down_right (row + 1) (col + 1) ((row, col) :: acc)
        init_color board
    else if get_color board.(row + 1).(col + 1) <> init_color then
      (row, col) :: (row + 1, col + 1) :: acc
    else (row, col) :: acc
  else (row, col) :: acc

let rec diagonal_down_left row col acc init_color board =
  if row < 7 && col > 0 then
    if get_piece board.(row + 1).(col - 1) = Empty then
      diagonal_down_left (row + 1) (col - 1) ((row, col) :: acc)
        init_color board
    else if get_color board.(row + 1).(col - 1) <> init_color then
      (row, col) :: (row + 1, col - 1) :: acc
    else (row, col) :: acc
  else (row, col) :: acc

(* Uses the helper methods in order to consolidate all of the diagonal
   positions given a certain piece *)
let diagonal row col init_color board =
  let lst =
    diagonal_down_left row col [] init_color board
    @ diagonal_up_right row col [] init_color board
    @ diagonal_up_left row col [] init_color board
    @ diagonal_down_right row col [] init_color board
  in
  List.filter (fun (x, y) -> x <> row || y <> col) lst

(* Uses the helper methods in order to find all of the possible
   horizontal and vertical moves that a piece can do.*)
let horiz_vert_posib row col init_color board =
  let lst =
    check_left_horz row col [] init_color board
    @ check_right_horz row col [] init_color board
    @ check_up_vert row col [] init_color board
    @ check_down_vert row col [] init_color board
  in
  List.filter (fun (x, y) -> x <> row || y <> col) lst

(* Updated so that if the piece is of the same color it cant move there. *)
let valid_features x y init_color board =
  x >= 0 && x <= 7 && y >= 0 && y <= 7
  && not (get_color board.(x).(y) = init_color)

(* Checks all of the places that the king can move to *)
let king_move row col init_color board =
  let lst =
    [
      (row - 1, col - 1);
      (row - 1, col);
      (row - 1, col + 1);
      (row, col - 1);
      (row, col + 1);
      (row + 1, col - 1);
      (row + 1, col);
      (row + 1, col + 1);
    ]
  in
  List.filter (fun (x, y) -> valid_features x y init_color board) lst

let print_coor x y =
  print_int x;
  print_string ",";
  print_int y

let knight_moves row col init_color board =
  (*print_coor row col;*)
  let lst =
    [
      (row + 2, col - 1);
      (row + 2, col + 1);
      (row - 2, col + 1);
      (row - 2, col - 1);
      (row + 1, col + 2);
      (row - 1, col + 2);
      (row + 1, col - 2);
      (row - 1, col - 2);
    ]
  in
  List.filter (fun (x, y) -> valid_features x y init_color board) lst

(*Helper method for black_pawn attacks. Looks at the left column for
  black pieces*)
let black_left_col row col board =
  if get_color board.(row + 1).(col + 1) = White then
    [ (row + 1, col + 1) ]
  else []

(*Helper method for black_pawn attacks. Looks at the right column for
  black pieces*)
let black_right_col row col board =
  if get_color board.(row + 1).(col - 1) = White then
    [ (row + 1, col - 1) ]
  else []

(*Helper method for black_pawn attacks. Looks at the middle columns for
  black pieces*)
let black_middle_col row col board =
  let lst = [] in
  if get_color board.(row + 1).(col + 1) = White then
    let lst2 = List.cons (row + 1, col + 1) lst in
    if get_color board.(row + 1).(col - 1) = White then
      List.cons (row + 1, col - 1) lst2
    else lst2
  else if get_color board.(row + 1).(col - 1) = White then
    [ (row + 1, col - 1) ]
  else []

(*helper for pawn_attacked_tiles. Return the coordinates a black pawn is
  attacking*)
let black_pawn_attacks row col board =
  if col = 0 then black_left_col row col board
  else if col = 7 then black_right_col row col board
  else black_middle_col row col board

(*Helper method for white_pawn attacks. Looks at the left column for
  white pieces*)
let white_left_col row col board =
  if get_color board.(row - 1).(col + 1) = Black then
    [ (row - 1, col + 1) ]
  else []

(*Helper method for white_pawn attacks. Looks at the right column for
  white pieces*)
let white_right_col row col board =
  if get_color board.(row - 1).(col - 1) = Black then
    [ (row - 1, col - 1) ]
  else []

(*Helper method for white_pawn attacks. Looks at the middle columns for
  white pieces*)
let white_middle_col row col board =
  let lst = [] in
  if get_color board.(row - 1).(col + 1) = Black then
    let lst2 = List.cons (row - 1, col + 1) lst in
    if get_color board.(row - 1).(col - 1) = Black then
      List.cons (row - 1, col - 1) lst2
    else lst2
  else if get_color board.(row - 1).(col - 1) = Black then
    [ (row - 1, col - 1) ]
  else []

(*Return the coordinates a white pawn is attacking *)
let white_pawn_attacks row col board =
  if col = 0 then white_left_col row col board
  else if col = 7 then white_right_col row col board
  else white_middle_col row col board

(*Removes the positions where a pawn is blocked by another piece*)
let rec remove_pawn_blocked row col board lst =
  match lst with
  | [] -> []
  | h :: t ->
      if
        h = (row - 2, col)
        && not (get_color board.(row - 2).(col) = None)
        || h = (row - 1, col)
           && not (get_color board.(row - 1).(col) = None)
        || h = (row + 1, col)
           && not (get_color board.(row + 1).(col) = None)
        || h = (row + 2, col)
           && not (get_color board.(row + 2).(col) = None)
      then remove_pawn_blocked row col board t
      else h :: remove_pawn_blocked row col board t

(*Return a list of coordinates for how a pawn at b.(row).(col) moves*)
let pawn_moves row col board =
  let color = get_color board.(row).(col) in
  if color = White && row = 6 then
    List.append
      [ (row - 1, col); (row - 2, col) ]
      (white_pawn_attacks row col board)
    |> remove_pawn_blocked row col board
  else if color = Black && row = 1 then
    List.append
      [ (row + 1, col); (row + 2, col) ]
      (black_pawn_attacks row col board)
    |> remove_pawn_blocked row col board
  else if color = White then
    List.append [ (row - 1, col) ] (white_pawn_attacks row col board)
    |> remove_pawn_blocked row col board
  else
    List.append [ (row + 1, col) ] (black_pawn_attacks row col board)
    |> remove_pawn_blocked row col board

(*White left col pawn attacking square*)
let white_left_col row col board = [ (row - 1, col + 1) ]

(*White right col pawn attacking square*)
let white_right_col row col board = [ (row - 1, col - 1) ]

(*White middle col pawn attacking squares*)
let white_middle_col row col board =
  [ (row - 1, col + 1); (row - 1, col - 1) ]

(*Helper for pawn_attacking_moves*)
let white_pawn_attacking_squares row col board =
  if col = 0 then white_left_col row col board
  else if col = 7 then white_right_col row col board
  else white_middle_col row col board

(*Black left col pawn attacking square*)
let black_left_col row col board = [ (row + 1, col + 1) ]

(*Black right col pawn attacking square*)
let black_right_col row col board = [ (row + 1, col - 1) ]

(*Black middle col pawn attacking squares*)
let black_middle_col row col board =
  [ (row + 1, col + 1); (row + 1, col - 1) ]

(*Helper for pawn_attacking_moves*)
let black_pawn_attacking_squares row col board =
  if col = 0 then black_left_col row col board
  else if col = 7 then black_right_col row col board
  else black_middle_col row col board

(*Returns a pawn's attacking moves*)
let pawn_attacking_moves row col b =
  let color = get_color b.(row).(col) in
  if color = Tile.White then white_pawn_attacking_squares row col b
  else black_pawn_attacking_squares row col b

(*Return true if the white king can castle left*)
let lw_rook_castle_condit board =
  k.white_king_moved = false
  && r.white_left_rook_moved = false
  && get_piece board.(7).(1) = Empty
  && get_piece board.(7).(2) = Empty
  && get_piece board.(7).(3) = Empty
  && (not (List.mem (7, 2) attacking_pos.black))
  && (not (List.mem (7, 3) attacking_pos.black))
  && not (List.mem (7, 4) attacking_pos.black)

(* Checks whether white king is able to castle right. *)
let rw_rook_castle_condit board =
  k.white_king_moved = false
  && r.white_right_rook_moved = false
  && get_piece board.(7).(5) = Empty
  && get_piece board.(7).(6) = Empty
  && (not (List.mem (7, 4) attacking_pos.black))
  && (not (List.mem (7, 5) attacking_pos.black))
  && not (List.mem (7, 6) attacking_pos.black)

(* Checks whether black king is able to castle left. *)
let lb_rook_castle_condit board =
  k.black_king_moved = false
  && r.black_left_rook_moved = false
  && get_piece board.(0).(1) = Empty
  && get_piece board.(0).(2) = Empty
  && get_piece board.(0).(3) = Empty
  && (not (List.mem (0, 2) attacking_pos.white))
  && (not (List.mem (0, 4) attacking_pos.white))
  && not (List.mem (0, 3) attacking_pos.white)

(* Checks whether black king is able to castle right. *)
let rb_rook_castle_condit board =
  k.black_king_moved = false
  && r.black_right_rook_moved = false
  && get_piece board.(0).(5) = Empty
  && get_piece board.(0).(6) = Empty
  && (not (List.mem (0, 4) attacking_pos.white))
  && (not (List.mem (0, 5) attacking_pos.white))
  && not (List.mem (0, 6) attacking_pos.white)

(*It returns the castle squares the king can go to*)
let castle x y board =
  match get_color board.(x).(y) with
  | White ->
      if lw_rook_castle_condit board && rw_rook_castle_condit board then
        [ (7, 2); (7, 6) ]
      else if lw_rook_castle_condit board then [ (7, 2) ]
      else if rw_rook_castle_condit board then [ (7, 6) ]
      else []
  | Black ->
      if lb_rook_castle_condit board && rb_rook_castle_condit board then
        [ (0, 2); (0, 6) ]
      else if lb_rook_castle_condit board then [ (0, 2) ]
      else if rb_rook_castle_condit board then [ (0, 6) ]
      else []
  | None -> []

(* Given a coordinate, it matches the piece type with the moves that the
   piece is able to do, and returns the possible moves. *)
let possible_moves x y board =
  let color = get_color board.(x).(y) in
  match get_piece board.(x).(y) with
  | King -> castle x y board @ king_move x y color board
  | Queen -> horiz_vert_posib x y color board @ diagonal x y color board
  | Bishop -> diagonal x y color board
  | Rook -> horiz_vert_posib x y color board
  | Knight -> knight_moves x y color board
  | Pawn -> pawn_moves x y board
  | Empty -> []

(*Returns the possible moves for each square not including kings*)
let possible_moves_without_king x y board =
  let color = get_color board.(x).(y) in
  match get_piece board.(x).(y) with
  | King -> []
  | Queen -> horiz_vert_posib x y color board @ diagonal x y color board
  | Bishop -> diagonal x y color board
  | Rook -> horiz_vert_posib x y color board
  | Knight -> knight_moves x y color board
  | Pawn -> pawn_moves x y board
  | Empty -> []

(*Attacking moves returns a list of the squares b.(x).(y) is attacking *)
let attacking_moves x y board =
  let color = get_color board.(x).(y) in
  match get_piece board.(x).(y) with
  | King -> king_move x y color board
  | Queen -> horiz_vert_posib x y color board @ diagonal x y color board
  | Bishop -> diagonal x y color board
  | Rook -> horiz_vert_posib x y color board
  | Knight -> knight_moves x y color board
  | Pawn -> pawn_attacking_moves x y board
  | Empty -> []

(*Returns attacking moves on the square x y not including kings*)
let attacking_moves_without_king x y board =
  let color = get_color board.(x).(y) in
  match get_piece board.(x).(y) with
  | King -> []
  | Queen -> horiz_vert_posib x y color board @ diagonal x y color board
  | Bishop -> diagonal x y color board
  | Rook -> horiz_vert_posib x y color board
  | Knight -> knight_moves x y color board
  | Pawn -> pawn_attacking_moves x y board
  | Empty -> []

let starterboard =
  "r,n,b,q,k,b,n,r/p,p,p,p,p,p,p,p/ , , , , , , , / , , , , , , , / , \
   , , , , , , / , , , , , , , /P,P,P,P,P,P,P,P/R,N,B,Q,K,B,N,R"

let string_to_lists (s : string) =
  let r = String.split_on_char '/' s in
  List.map (fun x -> String.split_on_char ',' x) r

let rec initialize_row (b : board) (r : int) (c : int) (p : string list)
    : unit =
  match p with
  | [] -> ()
  | h :: t ->
      b.(r).(c) <- parse_piece h r c;
      initialize_row b r (c + 1) t

(*Initialize the board with all pieces in initial positions. White
  pieces are on the bottom*)
let rec initialize (b : board) (s : string list list) (r : int) : unit =
  match s with
  | [] -> ()
  | h :: t ->
      initialize_row b r 0 h;
      initialize b t (r + 1)

let init : board = Array.make_matrix 8 8 empty_tile

let rec print_pairs lst =
  match lst with
  | [] -> print_string ""
  | h :: t ->
      print_int (fst h);
      print_string ",";
      print_int (snd h);
      print_string "\n";
      print_pairs t

(*Helper method for update_attackers*)
let rec update_attackers_row r c b wlst blst =
  match c with
  | 8 -> (wlst, blst)
  | y ->
      if get_color b.(r).(c) = White then
        update_attackers_row r (c + 1) b
          (wlst @ attacking_moves r c b)
          blst
      else if get_color b.(r).(c) = Black then
        update_attackers_row r (c + 1) b wlst
          (blst @ attacking_moves r c b)
      else update_attackers_row r (c + 1) b wlst blst

(*Returns a tuple of white's attacking squares and blacks attacking
  squares*)
let rec update_attackers r b wlst blst =
  match r with
  | 8 -> (wlst, blst)
  | x ->
      let tup = update_attackers_row x 0 b [] [] in
      update_attackers (r + 1) b (wlst @ fst tup) (blst @ snd tup)

(*Precondition: c is the color of the king*)
let king_attacked x2 y2 c =
  match c with
  | Tile.White -> List.mem (x2, y2) attacking_pos.black
  | Tile.Black -> List.mem (x2, y2) attacking_pos.white
  | _ -> failwith "impossible"

(*If moving a piece from b.(x).(y) to b.(x2).(y2) puts king in check,
  return true. Used in check_validity *)
let moving_in_check b x y x2 y2 c : bool =
  if get_piece b.(x).(y) = King then king_attacked x2 y2 c else false

(*Returns true if the c colored king is in check*)
let in_check c =
  match c with
  | Tile.White -> List.mem k.white_king attacking_pos.black
  | Tile.Black -> List.mem k.black_king attacking_pos.white
  | _ -> failwith "impossible"

(*Helper for lst_of_attackers*)
let rec lst_of_attackers_row r c b (lst : (int * int) list) ck =
  match c with
  | 8 -> lst
  | c -> (
      let color = get_color b.(r).(c) in
      match (ck, color) with
      | Tile.White, Tile.Black ->
          if List.mem k.white_king (attacking_moves r c b) then
            lst_of_attackers_row r (c + 1) b (lst @ [ (r, c) ]) ck
          else lst_of_attackers_row r (c + 1) b lst ck
      | Tile.Black, Tile.White ->
          if List.mem k.black_king (attacking_moves r c b) then
            lst_of_attackers_row r (c + 1) b (lst @ [ (r, c) ]) ck
          else lst_of_attackers_row r (c + 1) b lst ck
      | _ -> lst_of_attackers_row r (c + 1) b lst ck)

(*ck is the color of the king, returns the coordinates of the attackers
  on that king. Helper for is_checkmate and get_counter_squares.*)
let rec lst_of_attackers r b lst ck =
  match r with
  | 8 -> lst
  | x ->
      let l = lst_of_attackers_row x 0 b [] ck in
      lst_of_attackers (r + 1) b (lst @ l) ck

let tester_lst_attackers b =
  List.length (lst_of_attackers 0 b [] Tile.White)

(*Helper for in_line_with_king*)
let rook_line r c rk ck b [] =
  if r > rk then
    [ (r, c) ] @ check_up_vert r c [] (get_color b.(r).(c)) b
  else if r < rk then
    [ (r, c) ] @ check_down_vert r c [] (get_color b.(r).(c)) b
  else if c > ck then
    [ (r, c) ] @ check_left_horz r c [] (get_color b.(r).(c)) b
  else [ (r, c) ] @ check_right_horz r c [] (get_color b.(r).(c)) b

(*Helper for in_line_with_king*)
let bishop_line r c rk ck b [] =
  if r > rk && c < ck then
    (*Bottom left of king*)
    [ (r, c) ] @ diagonal_up_right r c [] (get_color b.(r).(c)) b
  else if r > rk && c > ck then
    (*Bottom right of king*)
    [ (r, c) ] @ diagonal_up_left r c [] (get_color b.(r).(c)) b
  else if r < rk && c < ck then
    (*Top left of king*)
    [ (r, c) ] @ diagonal_down_right r c [] (get_color b.(r).(c)) b
  else [ (r, c) ] @ diagonal_down_left r c [] (get_color b.(r).(c)) b

(*Helper for in_line_with_king*)
let queen_line r c rk ck b [] =
  if r = rk || c = ck then rook_line r c rk ck b []
  else bishop_line r c rk ck b []

(*Returns a bool of whether moving the king x y to x2 y2 is valid. x y
  is the king's position and trying to move to x2 y2. Helper_method for
  get_possible_king_moves Precondition: x and y represent the kings
  position *)
let can_move x y x2 y2 b =
  let c = get_color b.(x).(y) in
  let piece1 = b.(x).(y) in
  let piece2 = b.(x2).(y2) in
  b.(x2).(y2) <- piece1;
  b.(x).(y) <- empty_tile;

  let attacks = update_attackers 0 b [] [] in
  let battacks = snd attacks in
  let wattacks = fst attacks in

  match c with
  | Tile.White ->
      b.(x).(y) <- piece1;
      b.(x2).(y2) <- piece2;
      not (List.mem (x2, y2) battacks)
  | Tile.Black ->
      b.(x).(y) <- piece1;
      b.(x2).(y2) <- piece2;
      not (List.mem (x2, y2) wattacks)
  | None -> true

(*kmoves is a list of the possible moves for king including the checked
  squares, this method returns the possible moves without the checked
  squares. Helper for is_checkmate method. Precondition: r c is the
  king's position and lst is empty*)
let rec get_possible_king_moves r c kmoves lst b =
  match kmoves with
  | h :: t ->
      if can_move r c (fst h) (snd h) b then
        get_possible_king_moves r c t ([ h ] @ lst) b
      else get_possible_king_moves r c t lst b
  | [] -> lst

let tester_get_king_moves b =
  let r = fst k.white_king in
  let c = snd k.white_king in
  get_possible_king_moves r c (possible_moves r c b) [] b

(*Returns the king position given the color of the king*)
let get_king_pos ck =
  match ck with
  | Tile.White -> k.white_king
  | Tile.Black -> k.black_king
  | None -> failwith "impossible"

(*Returns coordinates that can block a check depending on the piece*)
let in_line_with_king r c ck b =
  let p = get_king_pos ck in
  match get_piece b.(r).(c) with
  | King -> failwith "impossible"
  | Knight -> [ (r, c) ]
  | Rook -> rook_line r c (fst p) (snd p) b []
  | Bishop -> bishop_line r c (fst p) (snd p) b []
  | Queen -> queen_line r c (fst p) (snd p) b []
  | Pawn -> [ (r, c) ]
  | Empty -> failwith "impossible"

(*The c colored king is in check. This method returns possible
  coordinates that will block the check or get rid of the check. Helper
  for counter_check method*)
let get_counter_squares (ck : Tile.c) (b : Tile.tile array array) :
    (int * int) list =
  if in_check ck then
    let clst = lst_of_attackers 0 b [] ck in
    let attacker = List.nth clst 0 in
    let r = fst attacker in
    let c = snd attacker in
    in_line_with_king r c ck b
  else []

(*Returns true if moving x y to x2 y2 puts the king out of check, false
  otherwise. Helper for check_validity*)
let counter_check x y x2 y2 b ck =
  match get_piece b.(x).(y) with
  | King -> (
      match ck with
      | Tile.White -> not (List.mem (x2, y2) attacking_pos.black)
      | Tile.Black -> not (List.mem (x2, y2) attacking_pos.white)
      | _ -> failwith "impossible")
  | _ -> List.mem (x2, y2) (get_counter_squares ck b)

(*Helper for all_possible_moves_without_king*)
let rec all_possible_moves_without_king_row r c b wlst blst =
  match c with
  | 8 -> (wlst, blst)
  | y ->
      if get_color b.(r).(c) = White then
        all_possible_moves_without_king_row r (c + 1) b
          (wlst @ possible_moves_without_king r c b)
          blst
      else if get_color b.(r).(c) = Black then
        all_possible_moves_without_king_row r (c + 1) b wlst
          (blst @ possible_moves_without_king r c b)
      else all_possible_moves_without_king_row r (c + 1) b wlst blst

(*Returns a (int * int) list of all the possible coordinates each player
  can go to *)
let rec all_possible_moves_without_king r b wlst blst =
  match r with
  | 8 -> (wlst, blst)
  | x ->
      let tup = all_possible_moves_without_king_row x 0 b [] [] in
      all_possible_moves_without_king (r + 1) b
        (wlst @ fst tup)
        (blst @ snd tup)

(*Helper method for can_block*)
let rec can_block_helper
    (all_moves : (int * int) list)
    (counter_squares : (int * int) list) =
  match all_moves with
  | h :: t ->
      if List.mem h counter_squares then true
      else can_block_helper t counter_squares
  | [] -> false

(*Given the color of the king ck, this method returns true if a same
  colored piece can block a check or take the piece checking the king*)
let can_block ck b =
  let counter_squares = get_counter_squares ck b in
  match ck with
  | Tile.White ->
      let all_moves = fst (all_possible_moves_without_king 0 b [] []) in
      can_block_helper all_moves counter_squares
  | Tile.Black ->
      let all_moves = snd (all_possible_moves_without_king 0 b [] []) in
      can_block_helper all_moves counter_squares
  | None -> true

let tester_can_block b = can_block Tile.White b

(*let counter_squares = get_counter_squares ck*)

(*Returns true if a player is in checkmate*)
let is_checkmate b =
  if in_check Tile.White then
    let r = fst k.white_king in
    let c = snd k.white_king in
    let kmoves = possible_moves r c b in
    let lst_attackers = lst_of_attackers 0 b [] Tile.White in
    List.length (get_possible_king_moves r c kmoves [] b) = 0
    &&
    if List.length lst_attackers > 1 then true
    else not (can_block Tile.White b)
  else if in_check Tile.Black then
    let r = fst k.black_king in
    let c = snd k.black_king in
    let kmoves = possible_moves r c b in
    let lst_attackers = lst_of_attackers 0 b [] Tile.Black in
    List.length (get_possible_king_moves r c kmoves [] b) = 0
    &&
    if List.length lst_attackers > 1 then true
    else not (can_block Tile.Black b)
  else false

(*This method returns true if the two coordinates are valid as a move *)
let check_validity
    (b : board)
    (x : int)
    (y : int)
    (x2 : int)
    (y2 : int)
    (c : int) : bool =
  let snd_tile = (x2, y2) in
  let bo =
    (not (get_piece b.(x).(y) = Empty))
    && ((get_color b.(x).(y) = Black && c mod 2 = 1)
       || (get_color b.(x).(y) = White && c mod 2 = 0))
    && List.mem snd_tile (possible_moves x y b)
    && (if in_check (get_color b.(x).(y)) then
        counter_check x y x2 y2 b (get_color b.(x).(y))
       else true)
    && not (moving_in_check b x y x2 y2 (get_color b.(x).(y)))
  in
  if bo then
    if get_color b.(x2).(y2) = White then (
      s.black_score <- s.black_score + point_value b.(x2).(y2);
      s.white_captured <- s.white_captured @ [ get_piece b.(x2).(y2) ];
      true)
    else if get_color b.(x2).(y2) = Black then (
      s.white_score <- s.white_score + point_value b.(x2).(y2);
      s.black_captured <- s.black_captured @ [ get_piece b.(x2).(y2) ];
      true)
    else true
  else false

let get_white_score = s.white_score

let get_black_score = s.black_score

let get_captured_black = s.black_captured

let get_captured_white = s.white_captured

(*This method is used if a pawn is at the first or last row of the board *)
let pawn_to_queen x2 y2 c b =
  match c with
  | Tile.White ->
      b.(x2).(y2) <- b.(fst q.white_queen).(snd q.white_queen)
  | Tile.Black ->
      b.(x2).(y2) <- b.(fst q.black_queen).(snd q.black_queen)
  | None -> failwith "impossible"

(* This method does the "castle" move. If the castle conditions are met,
   it moves the rook to its respective square.*)
let check_castle x y x2 y2 b =
  match ((x, y), (x2, y2)) with
  | (7, 4), (7, 2) ->
      b.(7).(3) <- b.(7).(0);
      b.(7).(0) <- empty_tile
  | (7, 4), (7, 6) ->
      b.(7).(5) <- b.(7).(7);
      b.(7).(7) <- empty_tile
  | (0, 4), (0, 2) ->
      b.(0).(3) <- b.(0).(0);
      b.(0).(0) <- empty_tile
  | (0, 4), (0, 6) ->
      b.(0).(5) <- b.(0).(7);
      b.(0).(7) <- empty_tile
  | _, _ -> ()

(*Helper for move_piece. Updates rooks*)
let update_rook_move x y x2 y2 b : unit =
  if get_piece b.(x).(y) = Rook then
    if r.white_left_rook = (x, y) then (
      r.white_left_rook <- (x2, y2);
      r.white_left_rook_moved <- true)
    else if r.white_right_rook = (x, y) then (
      r.white_right_rook <- (x2, y2);
      r.white_right_rook_moved <- true)
    else if r.black_left_rook = (x, y) then (
      r.black_left_rook <- (x2, y2);
      r.black_left_rook_moved <- true)
    else (
      r.black_right_rook <- (x2, y2);
      r.black_right_rook_moved <- true)
  else ()

(*Helper for move_piece. Updates kings*)
let update_king_move x y x2 y2 b =
  if get_piece b.(x).(y) = King then
    if get_color b.(x).(y) = White then (
      k.white_king <- (x2, y2);
      k.white_king_moved <- true)
    else (
      k.black_king <- (x2, y2);
      k.black_king_moved <- true)
  else ()

(*Helper for move_piece. Updates queens*)
let update_queen_move x y x2 y2 b =
  if get_piece b.(x).(y) = Queen then
    if get_color b.(x).(y) = White then q.white_queen <- (x2, y2)
    else q.black_queen <- (x2, y2)
  else ()

(*We call check_validity in main so we assume this move_piece takes
  valid positions*)
let move_piece (b : board) (x : int) (y : int) (x2 : int) (y2 : int) :
    bool =
  let c = get_color b.(x).(y) in

  update_king_move x y x2 y2 b;
  update_rook_move x y x2 y2 b;
  update_queen_move x y x2 y2 b;

  let piece1 = b.(x).(y) in
  let piece2 = b.(x2).(y2) in
  b.(x2).(y2) <- piece1;
  b.(x).(y) <- empty_tile;

  if get_piece piece1 = King then check_castle x y x2 y2 b else ();

  if get_piece piece1 = Pawn && (x2 = 0 || x2 = 7) then
    pawn_to_queen x2 y2 c b
  else ();

  let attacks = update_attackers 0 b [] [] in
  attacking_pos.white <- fst attacks;
  attacking_pos.black <- snd attacks;

  if in_check c then (
    b.(x).(y) <- piece1;
    b.(x2).(y2) <- piece2;
    let attacks = update_attackers 0 b [] [] in
    attacking_pos.white <- fst attacks;
    attacking_pos.black <- snd attacks;
    false)
  else true
