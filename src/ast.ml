type ident = string [@@deriving show]

type expr =
  | Ident of ident
  | Lambda of ident * expr
  | Abst of expr * expr
[@@deriving show]
