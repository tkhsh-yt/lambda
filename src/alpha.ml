open Tm

let find x env = try M.find x env with
                   Not_found -> (x, 0)

let rec alpha env = function
  | Var(x, id) ->
     let (x', id) = find x env in
     Var(x', id)
  | Name(n) ->
     Name(n)
  | Abs((x, _), e) ->
     let x' = Id.genid x in
     Abs(x', alpha (M.add x x' env) e)
  | App(e1, e2) ->
     App(alpha env e1, alpha env e2)
  | Assign(n, e) ->
     Assign(n, alpha env e)

let conv = alpha M.empty
