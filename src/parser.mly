%{
    open Token
    open Ast


    let merge_info linfo rinfo =
      {
        lnum_start = linfo.lnum_start;
        lnum_end   = rinfo.lnum_end;
        pos_start  = linfo.pos_start;
        pos_end    = rinfo.pos_end;
      }

    let make_abs x body =
      let (linfo, var) = x in
      let rinfo = Ast.get_info body in
      let info = merge_info linfo rinfo in
      Abs (info, Var (linfo, var), body)
%}

%token <Token.info * string> VAR
%token <Token.info> LPAREN RPAREN LAMBDA END
%token DOT EOF

%start main
%type <Ast.expr list> main

%start main_expr
%type <Ast.expr option> main_expr

%%

main:
  | list(stmt) EOF { $1 }

main_expr:
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
