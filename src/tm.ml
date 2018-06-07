type term =
  | Var of string * int
  | Name of string
  | Abs of (string * int) * term
  | App of term * term
  | Assign of string * term
[@@deriving show, eq, ord]

let rec from_ast = function
  | Ast.Var(_, x) ->
     Var(x, 0)
  | Ast.Name(_, n) -> Name(n)
  | Ast.Abs(_, (_, x), e) -> Abs((x, 0), from_ast e)
  | Ast.App(_, e1, e2) -> App(from_ast e1, from_ast e2)
  | Ast.Assign(_, n, e) -> Assign(n, from_ast e)

let rec show_pretty_term = function
  | Var(x, _) -> x
  | Name(n) -> n
  | Abs((x, _), e) -> "(λ" ^ x ^ "." ^ show_pretty_term e ^ ")"
  | App(e1, e2) -> "(" ^ show_pretty_term e1 ^ " " ^ show_pretty_term e2 ^ ")"
  | Assign(n, e) -> n ^ " := " ^ show_pretty_term e

