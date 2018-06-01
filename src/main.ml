
(* let () =
 *   let sexps = Parser.program Lexer.token (Lexing.from_channel stdin) in
 *   List.iter (fun sexp -> Sub.print_sexp stdout sexp; print_newline ()) sexps *)

let main () =
  Toplevel.main_loop ();;

main()
