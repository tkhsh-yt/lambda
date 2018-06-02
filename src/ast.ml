type ident = string [@@deriving show]

type expr =
  | Ident of ident
  | Abst of ident * expr
  | Apply of expr * expr
[@@deriving show]
