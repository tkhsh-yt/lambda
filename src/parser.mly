%{ open Ast %}

%token <string> Ident
%token Lambda Dot LParen RParen End Eof

%start main
%type <Ast.expr list> main

%start main_expr
%type <Ast.expr option> main_expr

%%

main:
  | exprs=list(expr) Eof { exprs }

main_expr:
  | expr=expr {Some expr}
  | Eof {None}

expr:
  | expr=iexpr End { expr }

iexpr:
  | ident=Ident { Ident ident }
  | LParen Lambda ident=Ident Dot expr=iexpr RParen { Lambda (ident, expr) }
  | LParen expl=iexpr expr=iexpr RParen { Abst (expl, expr) }
