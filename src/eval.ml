open Tm

let rec subst x s = function
  | Var(x') -> if x' = x then s else Var(x')
  | Abs(x', e) -> Abs(x', subst x s e)
  | App(e1, e2) -> App(subst x s e1, subst x s e2)
  | _ -> failwith ""

let find x env = try M.find x env with
                   Not_found -> failwith ("not found: " ^ x)

let rec eval_in env = function
  | Var(x) ->
     (Var(x), env)
  | Name(x) -> begin
      let (e', env') = eval_in env (find x env) in
      (e', env)
    end
  | Abs(v, e) ->
     let (e', _) = eval_in env e in
     (Abs(v, e'), env)
  | App(e1, e2) -> begin
      let (e2', _) = eval_in env e2 in
      let (e1', _) = eval_in env e1 in
      match e1' with
      | Var(_) -> (App(e1', e2'), env)
      | Abs(x, e') ->
         eval_in env (subst x e2' e')
      | App(_, _) -> (App(e1', e2'), env)
      | _ -> failwith ""
    end
  | Assign(n, e) ->
     (Assign(n, e), M.add n e env)

let eval = eval_in M.empty
