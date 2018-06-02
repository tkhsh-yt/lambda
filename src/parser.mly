%{ open Ast %}

%token <string> IDENT
%token LAMBDA DOT LPAREN RPAREN END EOF

%start main
%type <Ast.expr list> main

%start main_expr
%type <Ast.expr option> main_expr

%%

main:
  | stmts=list(stmt) EOF { stmts }

main_expr:
  | stmt=stmt { Some stmt }
  | EOF { None }

stmt:
  | expr=expr END { expr }

expr:
  | term=term { term }
  | expl=expr expr=term { Apply (expl, expr) }

term:
  | ident=IDENT { Ident ident }
  | LAMBDA ident=IDENT rest=list(IDENT) DOT expr=expr
    {
      let lambda = List.fold_left (fun acc x ->
                                    fun body -> acc (Abst (x, body)))
                                  (fun body -> Abst (ident, body))
                                  rest
      in lambda expr
    }
  | LPAREN expr=expr RPAREN { expr }
