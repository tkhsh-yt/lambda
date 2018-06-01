type lexbuf

val create_lexbuf:
  ?file:string -> Sedlexing.lexbuf -> lexbuf

val parse_program:
  lexbuf -> Ast.expr list

val parse_expr:
  lexbuf -> Ast.expr option

exception ParseError of (string * int * int * string)

val string_of_ParseError: (string * int * int * string) -> string
