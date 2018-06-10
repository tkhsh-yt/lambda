open Tm

let find x env = try M.find x env with
                   Not_found -> (x, 0)

let rec conv_in env = function
  | Var(x, id) ->
     let (x', id) = find x env in
     Var(x', id)
  | Name(n) ->
     Name(n)
  | Abs((x, _), e) ->
     let x' = Id.genid x in
     Abs(x', conv_in (M.add x x' env) e)
  | App(e1, e2) ->
     App(conv_in env e1, conv_in env e2)
  | Assign(n, e) ->
     Assign(n, conv_in env e)

let conv = conv_in M.empty
