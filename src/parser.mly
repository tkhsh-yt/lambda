%{
    open Ast


    let merge_info linfo rinfo =
      {
        lnum_start = linfo.lnum_start;
        lnum_end   = rinfo.lnum_end;
        pos_bol    = linfo.pos_bol;
        pos_cnum   = rinfo.pos_cnum;
      }

    let make_abs x body =
      let (linfo, var) = x in
      let rinfo = Ast.get_info body in
      let info = merge_info linfo rinfo in
      Abs (info, (linfo, var), body)
%}

%token <Ast.info * string> VAR
%token <Ast.info> LPAREN RPAREN LAMBDA END
%token DOT EOF

%start main
%type <Ast.expr list> main

%start main_stmt
%type <Ast.expr option> main_stmt

%%

main:
  | list(stmt) EOF { $1 }

main_stmt:
  | stmt { Some $1 }
  | EOF  { None }

stmt:
  | expr END { $1 }

expr:
  | term { $1 }
  | expr term
    {
      let linfo = Ast.get_info $1 in
      let rinfo = Ast.get_info $2 in
      let info = merge_info linfo rinfo in
      App (info, $1, $2)
    }

term:
  | VAR
    {
      let (info, var) = $1 in
      Var (info, var)
    }
  | LAMBDA VAR list(VAR) DOT expr
    {
      let lambda = List.fold_left
                     (fun acc x ->
                       fun body -> acc (make_abs x body))
                     (fun body -> make_abs $2 body)
                     $3
      in lambda $5
    }
  | LPAREN expr RPAREN { $2 }
