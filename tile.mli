(** Representation of the tiles and pieces on a chessboard

    This module represents the state of each tile on the chessboard:
    whether there is a piece on it and if so, what kind, the colors of
    the pieces and a link to an img that that tile type.*)
open Js_of_ocaml

(** Abstract type for the tiles on a chessboard*)
type tile

(** Types of pieces on a tile*)
type p =
  | Empty
  | Rook
  | Bishop
  | Knight
  | Pawn
  | Queen
  | King

(** Color of the piece on a tile*)
type c =
  | None
  | White
  | Black

(** img for a piece on a tile*)
type img =
  | Image of string
  | No

(** [point_value t] represents the point value of t. *)
val point_value : tile -> int

(** [print_piece p] prints a string representation of the piece p. *)
val print_piece : p -> unit

(** [get_piece t] returns the piece on tile t. *)
val get_piece : tile -> p

(** [get_color t] returns the color on tile t. *)
val get_color : tile -> c

(** [get_img t] returns the image of tile t. *)
val get_img : tile -> img

(** [empty_tile] is the empty tile. *)
val empty_tile : tile

(** [pawn] is the pawn tile. *)
val pawn : tile

(** parse_piece s x y returns the tile with corresponding piece, color,
    img, of the string s.*)
val parse_piece : string -> int -> int -> tile
