type info = {
    lnum_start: int;
    lnum_end  : int;
    pos_start : int;
    pos_end   : int;
  } [@@deriving show]

(* type token =
 *   | LAMBDA of info
 *   | DOT
 *   | LPAREN of info
 *   | RPAREN of info
 *   | END of info
 *   | VAR of info * string
 *   | EOF *)
