type term =
  | Var of string
  | Abs of string * term
  | App of term * term
[@@deriving show]

let rec from_ast = function
  | Ast.Var(_, x) -> Var(x)
  | Ast.Abs(_, (_, x), e) -> Abs(x, from_ast e)
  | Ast.App(_, e1, e2) -> App(from_ast e1, from_ast e2)

let rec show_pretty_term = function
  | Var(x) -> x
  | Abs(x, e) -> "(λ" ^ x ^ "." ^ show_pretty_term e ^ ")"
  | App(e1, e2) -> "(" ^ show_pretty_term e1 ^ " " ^ show_pretty_term e2 ^ ")"

let rec subst x s = function
  | Var(x') -> if x' = x then s else Var(x')
  | Abs(x', e) -> Abs(x', subst x s e)
  | App(e1, e2) -> App(subst x s e1, subst x s e2)

let rec eval = function
  | Var(v) -> Var(v)
  | Abs(v, e) -> Abs(v, eval e)
  | App(e1, e2) ->
     let e2' = eval e2 in
     let e1' = eval e1 in
     match e1' with
     | Var(_) -> App(e1', e2')
     | Abs(x, e') -> eval (subst x e2' e')
     | App(_, _) -> App(e1', e2')

