open Util
open Tm

let rec subst x s = function
  | Var(x', id) -> if (x', id) = x then s else Var(x', id)
  | Abs(x', e) -> Abs(x', subst x s e)
  | App(e1, e2) -> App(subst x s e1, subst x s e2)
  | _ -> failwith "unreachable code"

let find x env = try M.find x env with
                   Not_found -> failwith @@ "not found NAME: " ^ x

let rec reduct_in env = function
  | Var(x, id) ->
     (Var(x, id), env)
  | Name(x) -> begin
      let (e', _) = reduct_in env (Alpha.conv @@ find x env) in
      (e', env)
    end
  | Abs(v, e) ->
     let (e', _) = reduct_in env e in
     (Abs(v, e'), env)
  | App(e1, e2) -> begin
      let (e1', _) = reduct_in env e1 in
      let (e2', _) = reduct_in env e2 in
      match e1' with
      | Var(_) -> (App(e1', e2'), env)
      | Abs(x, e') ->
         reduct_in env @@ subst x e2' e'
      | App(_, _) -> (App(e1', e2'), env)
      | _ -> failwith "unreachable code"
    end
  | Assign(n, e) ->
     (Assign(n, e), M.add n e env)

let reduct = reduct_in M.empty
