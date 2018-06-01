let rec main_loop () : unit =
  Printf.printf "λ> %!";
  match
    let lexbuf = Syntax.create_lexbuf @@
      Sedlexing.Utf8.from_channel stdin in
    Syntax.parse_expr lexbuf
  with
  | Some expr -> begin
      print_endline @@ Ast.show_expr expr;
      main_loop ()
    end

  | exception Syntax.ParseError e -> begin
      print_endline @@ Syntax.string_of_ParseError e;
      main_loop ()
    end

  | None -> print_newline ()

