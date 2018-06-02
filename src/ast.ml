type ident = string

type 'a expr =
  | Ident of 'a
  | Abst of 'a * 'a expr
  | Apply of 'a expr * 'a expr

let rec show_expr = function
  | Ident ident -> ident
  | Abst (ident, expr) -> "(λ" ^ ident ^ "." ^ show_expr expr ^ ")"
  | Apply (expl, expr) -> "(" ^ show_expr expl ^ " " ^ show_expr expr ^ ")"
