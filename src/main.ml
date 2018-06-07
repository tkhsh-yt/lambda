open Ast

let interpret file =
  let ic = open_in file in
  let lexbuf = Syntax.create_lexbuf @@ Sedlexing.Utf8.from_channel ic in
  try
    let rec parse () =
      let exprs = Syntax.parse_program lexbuf in
      let terms = List.map (fun e -> Tm.from_ast e) exprs in
      let alpha = List.map (fun e -> Alpha.conv e) terms in
      let eval = List.map (fun e -> Tm.eval e) alpha in
      List.iter (fun e -> print_endline (Tm.show_pretty_term e)) eval in
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
