open Ast

module S = Set.Make(String)

let rec pat_fv = function
  | Pint _ | Pbool _ | Pignore -> S.empty
  | Pvar x -> S.singleton x
  | Pdata(tag, ps) -> List.fold_left (fun z p -> S.union z @@ pat_fv p) S.empty ps

let rec subs x e = function
  | Lam(y, body) when x <> y ->
    Lam(y, subs x e body)
  | App(f, a) -> App(subs x e f, subs x e a)
  | Let(y, bde, body) when x <> y ->
    Let(y, subs x e bde, subs x e body)
  | Letrec(y, bde, body) when x <> y ->
    Letrec(y, subs x e bde, subs x e body)
  | Data(tag, args) ->
    Data(tag, List.map (subs x e) args)
  | Case c -> Case begin match c with
      | P(p, e') when not @@ S.mem x @@ pat_fv p -> P(p, subs x e e')
      | Pwhen(p, pred, e') when not @@ S.mem x @@ pat_fv p -> Pwhen(p, subs x e pred, subs x e e')
      | _ -> c
    end
  | Match(e', cs) ->
    let cs' = cs |> List.map @@ function
      | P(p, e') when not @@ S.mem x @@ pat_fv p -> P(p, subs x e e')
      | Pwhen(p, pred, e') when not @@ S.mem x @@ pat_fv p -> Pwhen(p, subs x e pred, subs x e e')
      | c -> c
    in
    Match(subs x e e', cs')
  | u -> u

let rec eval env = function
  | Int i -> Vint i
  | Bool b -> Vbool b
  | BinOp(op, l, r) ->
    let l' = eval env l and r' = eval env r in
    eval_binop op l' r'
  | UniOp(op, e) ->
    let e' = eval env e in
    eval_uniop op e'
  | Var x ->
    begin match List.assoc_opt x env with
      | Some v -> v
      | None -> failwith @@ Printf.sprintf "not found `%s'" x
    end
  | Lam(x, body) ->
    Vlam(x, ref env, body)
  | App(f, e) ->
    let e' = eval env e and f' = eval env f in
    begin match f' with
      | Vlam(x, { contents = env'}, body) -> eval env' @@ subs x e' body
      | _ -> failwith @@ Printf.sprintf "not a function `%s'" @@ show_v f'
    end
  | Let(x, bde, body) ->
    let bde' = eval env bde in
    eval ((x, bde') :: env) body
  | Letrec(x, bde, body) ->
    let bde' = eval env bde in
    begin match bde' with
      | Vlam(lx, lenv, lbody) ->
        let r = lenv in
        let env' = (x, Vlam(lx, r, lbody)) :: !lenv in
        r := env';
        eval env' body
      | _ -> eval ((x, bde') :: env) body
    end
  | Data(cstr, es) -> Vdata(cstr, List.map (eval env) es)
  | Case c -> Vcase c
  | Match(e, cases) ->
    let e' = eval env e in
    let env', body = get_pattern env e' cases in
    eval env' body
and eval_binop op l r =
  let iop f l r = match l, r with Vint l, Vint r -> Vint (f l r) | _ -> failwith "ibinop"
  and bop f l r = match l, r with Vbool l, Vbool r -> Vbool (f l r) | _ -> failwith "bbinop"
  in
  match op with
  | `Add -> iop (+) l r
  | `Sub -> iop (-) l r
  | `Mul -> iop ( * ) l r
  | `Div -> iop (/) l r
  | `Lt -> bop (<) l r
  | `Gt -> bop (>) l r
  | `Eq -> Vbool (l = r)
  | `Neq -> Vbool (l <> r)
  | `And -> bop (&&) l r
  | `Or -> bop (||) l r
and eval_uniop op v =
  let iop f = function (Vint i) -> Vint(f i) | _ -> failwith "iuniop"
  and bop f = function (Vbool b) -> Vbool(f b) | _ ->failwith "buniop"
  in
  match op with
  | `Not -> bop not v
  | `Minus -> iop (~-) v
and get_pattern_env env v p =
  match v, p with
  | Vlam(_, _, _), _ | Vcase(_), _ | _, Pignore -> env
  | _, Pvar x -> (x, v) :: env
  | Vdata(cstr, vs), Pdata(cstr', ps) when cstr = cstr' ->
    List.fold_left2 (fun z v p ->
        get_pattern_env z v p
      ) env vs ps
  | _, _ -> env
and get_pattern env v = function
  | [] -> raise @@ Match_failure("get_pattern", 0, 0)
  | p :: ps ->
    try
      match v with
      | Vint i ->
        begin match p with
          | P(Pint i', body) when i = i' -> env, body
          | Pwhen(Pint i', pred, body) when i = i' ->
            if eval env pred = Vbool true then env, body
            else raise_notrace Exit
          | _ -> raise_notrace Exit
        end
      | Vbool b ->
        begin match p with
          | P(Pbool b', body) when b = b' -> env, body
          | Pwhen(Pbool b', pred, body) when b = b' ->
            if eval env pred = Vbool true then env, body
            else raise_notrace Exit
          | _ -> raise_notrace Exit
        end
      | Vdata(cstr, vs) ->
        begin match p with
          | P(Pdata(cstr', ps), body) when cstr = cstr' ->
            let env' = get_pattern_env env v @@ Pdata(cstr', ps) in
            if List.(length env' = length env) then raise_notrace Exit
            else env', body
          | Pwhen(Pdata(cstr', ps), pred, body) when cstr = cstr' ->
            let env' = get_pattern_env env v @@ Pdata(cstr', ps) in
            if List.(length env' > length env) && eval env' pred = Vbool true then
              env', body
            else raise_notrace Exit
          | _ -> raise_notrace Exit
        end
      | _ -> failwith @@ Printf.sprintf "not match-able object: %s" @@ show_v v
    with Exit -> get_pattern env v ps

