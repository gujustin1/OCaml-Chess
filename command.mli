(** Parsing terminal commands for terminal version of Chess

    Module includes functions to parse terminal inputs for Chess*)

(** Abstract implmentation of console command *)
type object_phrase = string list

(** 3 types for a command*)
type command =
  | Move of object_phrase
  | InvalidMove
  | Quit

(** [parse s] returns s as a command*)
val parse : string -> command
