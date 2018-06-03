open Token

type ident = string

type expr =
  | Var of  info * string
  | Abs of info * expr * expr
  | App of info * expr * expr
[@@deriving show]

let get_info = function
  | Var (info, _) -> info
  | Abs (info, _, _) -> info
  | App (info, _, _) -> info

let rec pretty_show_expr = function
  | Var (_, var) -> var
  | Abs (_, Var(_, ident), expr) -> "(λ" ^ ident ^ "." ^ show_expr expr ^ ")"
  | App (_, expl, expr) -> "(" ^ show_expr expl ^ " " ^ show_expr expr ^ ")"
  | _ ->  failwith "undefined"

(* type 'a expr =
 *   | Var of  'a
 *   | Abs of 'a * 'a expr
 *   | Apply of 'a expr * 'a expr
 * 
 * let rec show_expr = function
 *    | Var (var) -> var
 *    | Abs (ident, expr) -> "(λ" ^ ident ^ "." ^ show_expr expr ^ ")"
 *    | Apply (expl, expr) -> "(" ^ show_expr expl ^ " " ^ show_expr expr ^ ")" *)
