type info = {
    lnum_start: int;
    lnum_end  : int;
    pos_bol   : int;
    pos_cnum  : int;
  } [@@deriving show]

type expr =
  | Var of info * string
  | Abs of info * (info * string) * expr
  | App of info * expr * expr
[@@deriving show]

let get_info = function
  | Var (info, _) -> info
  | Abs (info, _, _) -> info
  | App (info, _, _) -> info

let rec show_pretty_expr = function
  | Var(_, x) -> x
  | Abs(_, (_, x), e) -> "(λ" ^ x ^ "." ^ show_pretty_expr e ^ ")"
  | App(_, e1, e2) -> "(" ^ show_pretty_expr e1 ^ " " ^ show_pretty_expr e2 ^ ")"
