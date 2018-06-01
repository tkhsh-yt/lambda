open Ast

let interpret file =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  try
    let rec parse () =
      let res = Parser.main Lexer.token lexbuf in
      List.iter (fun s -> print_string (show_expr s); print_newline ()) res in
    parse ()
  with
  | Parser.Error ->
     exit 1

let help () = print_string "lambda <file>\n"

let () = if Array.length Sys.argv = 1 then help ()
         else
           let file = Array.get Sys.argv 1 in
           interpret file
