{
  open Token
  open Lexing
  open Parser

  exception SyntaxError of string

  let get_pos lexbuf = {
      lnum_start  = lexbuf.lex_curr_p.pos_lnum;
      lnum_end    = lexbuf.lex_curr_p.pos_lnum;
      pos_start   = Lexing.lexeme_start lexbuf;
      pos_end     = Lexing.lexeme_end lexbuf;
  }
}

let space = [' ' '\t' '\n' '\r']

let ident = ['a'-'z' '_']

rule token = parse
| ident* { VAR (get_pos lexbuf, Lexing.lexeme lexbuf) }
| '('    { LPAREN (get_pos lexbuf) }
| ')'    { RPAREN (get_pos lexbuf) }
| '.'    { DOT }
| '\\'   { LAMBDA (get_pos lexbuf) }
| ';'    { END (get_pos lexbuf) }
| space+ { token lexbuf }
| eof    { EOF }
| _
    {
      let message = Printf.sprintf
                      "%d: unknown token %s near characters %d-%d"
                      (lexbuf.lex_curr_p.pos_lnum)
                      (Lexing.lexeme lexbuf)
                      (Lexing.lexeme_start lexbuf)
                      (Lexing.lexeme_end lexbuf)
      in
      raise (SyntaxError message)
    }
