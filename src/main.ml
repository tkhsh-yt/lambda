open Ast

let interpret file =
  let ic = open_in file in
  let lexbuf = Syntax.create_lexbuf @@ Sedlexing.Utf8.from_channel ic in
  try
    let rec parse () =
      let res = Syntax.parse_program lexbuf in
      List.iter (fun s -> print_endline (show_expr s)) res in
    parse ()
  with
  (* | SyntaxError msg ->
   *    print_endline msg;
   *    exit 1; *)
  | Parser.Error ->
     print_string "Error";
     print_newline ();
     exit 1

let help () = print_string "lambda <file>\n"

let () = if Array.length Sys.argv = 1 then help ()
         else
           let file = Array.get Sys.argv 1 in
           interpret file
