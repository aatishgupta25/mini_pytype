(** constraint_gen.ml
    Walk an [Ast.module_] and collect type‐equality constraints.
    Step 2 only *builds* the list; Step 3 will bring the solver.
*)

open Ast

(* ───── A constraint is just τ₁ = τ₂ ──────────────────────────────────── *)
type constr = ty * ty

(* Environment = mapping from names to their most recent type binding ── *)
module Env = Map.Make (String)

(* ───── Register built‐ins ───────────────────────────────────────────── *)
let builtin_env =
  Env.empty
  |> Env.add "len" (TFunc ([TAny], TInt))
  (* Add more built‐ins here as needed *)

(* ───── The checker’s evolving state ─────────────────────────────────── *)
type state = {
  env     : ty Env.t;     (* variable, function, and built‐in bindings *)
  next_id : int;          (* for fresh TVar *)
  constrs : constr list;  (* accumulated constraints *)
  ret_ty  : ty option     (* the enclosing function’s declared return type *)
}

let fresh_var st =
  let v = TVar st.next_id in
  (v, { st with next_id = st.next_id + 1 })

(* ───── Expression traversal ──────────────────────────────────────────── *)
let rec gen_expr (st : state) (e : expr) : ty * state =
  match e with
  | ELitInt _   -> (TInt, st)
  | ELitFloat _ -> (TFloat, st)
  | ELitBool _  -> (TBool, st)
  | ELitStr _   -> (TStr, st)

  | EName x ->
    (match Env.find_opt x st.env with
     | Some t -> (t, st)
     | None   -> (TAny, st))  (* unbound name → Any *)

  | EList els ->
    let (elem_ty, st1) = fresh_var st in
    let st2 =
      List.fold_left
        (fun acc el ->
           let (ty_el, acc') = gen_expr acc el in
           { acc' with constrs = (elem_ty, ty_el) :: acc'.constrs })
        st1
        els
    in
    (TList elem_ty, st2)

  | EDict pairs ->
    let (k_ty, st1) = fresh_var st in
    let (v_ty, st2) = fresh_var st1 in
    let st3 =
      List.fold_left
        (fun acc (k, v) ->
           let (tk, acc')  = gen_expr acc k in
           let (tv, acc'') = gen_expr acc' v in
           { acc'' with constrs = (k_ty, tk) :: (v_ty, tv) :: acc''.constrs })
        st2
        pairs
    in
    (TDict (k_ty, v_ty), st3)

  | EBinOp (_op, e1, e2) ->
    let (t1, s1) = gen_expr st e1 in
    let (t2, s2) = gen_expr s1 e2 in
    let (res_ty, s3) = fresh_var s2 in
    let s4 = { s3 with constrs = (t1, t2) :: (t1, res_ty) :: s3.constrs } in
    (res_ty, s4)

  | ECall (fn_expr, arg_exprs) ->
    (* 1. Infer the function’s type *)
    let (fn_ty, s1) = gen_expr st fn_expr in

    (* 2. Infer each argument’s type *)
    let (arg_tys, s2) =
      List.fold_left
        (fun (tys_acc, acc) arg ->
           let (ty_arg, acc') = gen_expr acc arg in
           (tys_acc @ [ty_arg], acc'))
        ([], s1)
        arg_exprs
    in

    (* 3. If it’s a matching TFunc, emit constraints; otherwise degrade to Any *)
    begin match fn_ty with
    | TFunc (param_tys, ret_ty) when List.length param_tys = List.length arg_tys ->
        let s3 =
          List.fold_left2
            (fun acc expected actual ->
               { acc with constrs = (actual, expected) :: acc.constrs })
            s2
            param_tys
            arg_tys
        in
        (ret_ty, s3)
    | TFunc _ ->
        (* wrong arity → no crash, unknown result *)
        (TAny, s2)
    | _ ->
        (* non‐function call → unknown result *)
        (TAny, s2)
    end

  | EAttr (obj, name) ->
    (* Simple method lookup: treat `obj.method` via environment key `name` *)
    let (_, s1) = gen_expr st obj in
    let m_ty =
      Env.find_opt name s1.env
      |> Option.value ~default:TAny
    in
    (m_ty, s1)

  | ESubscript (e1, e2) ->
    (* Subscription yields a fresh type variable *)
    let (_, s1) = gen_expr st e1 in
    let (_, s2) = gen_expr s1 e2 in
    fresh_var s2

(* ───── Statement traversal ──────────────────────────────────────────── *)
and gen_stmt (st : state) (s : stmt) : state =
  match s with
  | SExpr e ->
      let (_, st') = gen_expr st e in
      st'

  | SAssign (targets, value) ->
      let (rhs_ty, st1) = gen_expr st value in
      List.fold_left
        (fun acc t ->
           match t with
           | EName x ->
             let acc' = { acc with env = Env.add x rhs_ty acc.env } in
             { acc' with constrs = (rhs_ty, rhs_ty) :: acc'.constrs }
           | _ -> acc)
        st1
        targets

  | SReturn None ->
      st  (* bare `return` or end‐of‐function → ignore *)

  | SReturn (Some e) ->
      let (ty_ret, st1) = gen_expr st e in
      (match st1.ret_ty with
       | Some declared ->
           { st1 with constrs = (ty_ret, declared) :: st1.constrs }
       | None -> st1)

  | SFuncDef func ->
      (* 1. Register the function’s signature *)
      let param_tys =
        List.map (fun (_, o) -> Option.value ~default:TAny o) func.params
      in
      let declared_ret = Option.value ~default:TAny func.returns in
      let f_ty = TFunc (param_tys, declared_ret) in
      let st1 = {
        st with
        env    = Env.add func.fname f_ty st.env;
        ret_ty = Some declared_ret
      } in

      (* 2. Generate constraints for its body *)
      List.fold_left gen_stmt st1 func.body

  | SClassDef cls ->
      (* Register each method by name so EAttr can find it *)
      List.fold_left
        (fun acc m ->
           let m_params = List.map (fun (_, o) -> Option.value ~default:TAny o) m.params in
           let m_ret    = Option.value ~default:TAny m.returns in
           let method_ty = TFunc (m_params, m_ret) in
           { acc with env = Env.add m.fname method_ty acc.env })
        st
        cls.methods

(* ───── Module entry point ───────────────────────────────────────────── *)
let gen_module (m : module_) : constr list =
  let init = {
    env     = builtin_env;
    next_id = 0;
    constrs = [];
    ret_ty  = None
  } in
  let final = List.fold_left gen_stmt init m in
  List.rev final.constrs
