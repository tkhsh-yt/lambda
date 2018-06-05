type info = {
    lnum_start: int;
    lnum_end  : int;
    pos_bol   : int;
    pos_cnum  : int;
  } [@@deriving show]

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
