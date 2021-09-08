(** Chessboard and all the functions on a board

    Module represents the state of a chessboard and includes all
    functions that can be made on a board such as checking for
    checkmate, moving a piece, and initializing chessboards*)

(** The abstract type that represents a chessboard. *)
type board = Tile.tile array array

(** The abstract type that represents a scoreboard.*)
type scoreboard

(** Raised when an invalid move attempted*)
exception InvalidMove

(** [possible_moves x y b] is a set-like list of all the tiles that a
    piece at position (x,y) can move to.*)
val possible_moves : int -> int -> board -> (int * int) list

(** [string_to_lists s] is a string list list representation of s, where
    each string list is created by splitting on character '/' in s. *)
val string_to_lists : string -> string list list

(** [starterboard] is the initial board state of a chess game as a
    string. *)
val starterboard : string

(** [initialize b s 0] turns the string representation, s, of the board
    into a board of tiles, b. *)
val initialize : board -> string list list -> int -> unit

(** [init] is the empty board.*)
val init : board

(** [get_white_score] is white's score.*)
val get_white_score : int

(** [get_black_score] is blacks's score.*)
val get_black_score : int

(** [get_captured_white] is a list of the captured white pieces.*)
val get_captured_white : Tile.p list

(** [get_captured_white] is a list of the captured black pieces.*)
val get_captured_black : Tile.p list

(**Used to print in the text game, helps debug*)
val tester_get_king_moves : board -> (int * int) list

(**Used to print in the text game, helps debug*)
val tester_can_block : board -> bool

(**Used to print in the text game, helps debug*)
val tester_lst_attackers : board -> int

(** [is_checkmate b] returns [true] if a king is in checkmate, and
    [false] otherwise.*)
val is_checkmate : board -> bool

(** [in_check c] returns whether the c colored king is in check.*)
val in_check : Tile.c -> bool

(** [check_validity b x y x2 y2 c] is [true] iff moving (x,y) to (x2,y2)
    is a valid move in the state of the game. Examples of returning
    [false] includes moving another piece when the king is in check,
    moving the king into a checked square, and castling in failed
    conditions. *)
val check_validity : board -> int -> int -> int -> int -> int -> bool

(** [move_piece b x1 y1 x2 y2] moves the piece on b.(x1).(y1) to
    b.(x2).(y2). There is a precondition that the move is valid.*)
val move_piece : board -> int -> int -> int -> int -> bool
