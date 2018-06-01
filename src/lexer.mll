{
  open Token
  open Lexing
  open Parser

  exception SyntaxError of string
}

let space = [' ' '\t' '\n' '\r']

let ident = ['a'-'z' '_']

rule token = parse
| ident* { IDENT (Lexing.lexeme lexbuf) }
| '('    { LPAREN }
| ')'    { RPAREN }
| '.'    { DOT }
| '\\'   { LAMBDA }
| ';'    { END }
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
