%{ open Ast %}

%token <string> IDENT
%token LAMBDA DOT LPAREN RPAREN END EOF

%start main
%type <Ast.expr list> main

%start main_expr
%type <Ast.expr option> main_expr

%%

main:
  | exprs=list(expr) EOF { exprs }

main_expr:
  | expr=expr {Some expr}
  | EOF {None}

expr:
  | expr=iexpr END { expr }

iexpr:
  | ident=IDENT { Ident ident }
  | LPAREN LAMBDA ident=IDENT DOT expr=iexpr RPAREN { Lambda (ident, expr) }
  | LPAREN expl=iexpr expr=iexpr RPAREN { Abst (expl, expr) }
