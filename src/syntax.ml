type lexbuf = {
  stream: Sedlexing.lexbuf;
  mutable pos: Lexing.position;
}

let create_lexbuf ?(file="") stream =
  let pos = {Lexing.
              pos_fname = file;
              pos_lnum = 1;
              pos_bol = 0;
              pos_cnum = 0;
            }
  in {pos; stream}

let new_line ?(n=0) lexbuf =
  let open Lexing in
  let lcp = lexbuf.pos in
  lexbuf.pos <-
    {lcp with
     pos_lnum = lcp.pos_lnum + 1;
     pos_bol = lcp.pos_cnum;
    }

let update lexbuf =
  let new_pos = Sedlexing.lexeme_end lexbuf.stream in
  let p = lexbuf.pos in
  lexbuf.pos <- {p with Lexing.pos_cnum = new_pos}

let lexeme {stream} = Sedlexing.Utf8.lexeme stream

(** [ParseError (file, line, col, token)] *)
exception ParseError of (string * int * int * string)

let raise_ParseError lexbuf =
  let {pos} = lexbuf in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  let tok = lexeme lexbuf in
  raise @@ ParseError (pos.pos_fname, line, col, tok)

let string_of_ParseError (file, line, cnum, tok) =
  let file_to_string file =
    if file = "" then ""
    else " on file " ^ file
  in
  Printf.sprintf
    "Parse error%s line %i, column %i, token %s"
    (file_to_string file)
    line cnum tok

(** Sedlexの構文に従ってトークン表現を定義 *)
(** 記号のトークン表現 **)
let exp_ident = [%sedlex.regexp? lowercase | Chars "#$_"]

(** ラムダのトークン表現 **)
let exp_lambda = [%sedlex.regexp? '\\']

(**終了のトークン表現 **)
let exp_end = [%sedlex.regexp? ';']

(** 字句解析 *)
let rec lex lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with

  (** 改行 *)
  | '\n' ->
    update lexbuf; new_line lexbuf;
    lex lexbuf

  (** 空白文字 *)
  | white_space ->
    update lexbuf;
    lex lexbuf

  | exp_lambda ->
     update lexbuf;
     Parser.Lambda

  | '.' ->
     update lexbuf;
     Parser.Dot

  | exp_ident ->
    update lexbuf;
    Parser.Ident (lexeme lexbuf)

  | exp_end ->
     update lexbuf;
     Parser.End

  (** コメント *)
  | "--", Star (Compl '\n'), '\n' ->
    update lexbuf; new_line lexbuf;
    lex lexbuf

  | eof ->
    update lexbuf;
    Parser.Eof

  | '(' -> update lexbuf; Parser.LParen
  | ')' -> update lexbuf; Parser.RParen

  | _ ->
    update lexbuf;
    raise_ParseError lexbuf


let parse f lexbuf =
  let lexer () =
    let ante_position = lexbuf.pos in
    let token = lex lexbuf in
    let post_position = lexbuf.pos
    in (token, ante_position, post_position) in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised f
  in
  try
    parser lexer
  with
  | Parser.Error
  | Sedlexing.MalFormed
  | Sedlexing.InvalidCodepoint _
    -> raise_ParseError lexbuf

let parse_program lexbuf =
  parse Parser.main lexbuf


let parse_expr lexbuf =
  parse Parser.main_expr lexbuf
