(** unify.ml
    Hindley–Milner unification with union–find and occurs check,
    rewritten to operate over Ast.ty directly.
*)

open Ast

(* Substitution: type-variable id → concrete Ast.ty *)
module IntMap = Map.Make (Int)
type subst = ty IntMap.t

exception Unify_error of ty * ty

(* ------------------------------------------------------------------ *)
(* Simple union–find over TVar ids                                    *)
(* ------------------------------------------------------------------ *)
module UF = struct
  let parent : (int, int) Hashtbl.t = Hashtbl.create 31

  let rec find v =
    match Hashtbl.find_opt parent v with
    | None ->
        v
    | Some v' when v' = v ->
        v
    | Some v' ->
        let r = find v' in
        Hashtbl.replace parent v r;
        r

  let union a b =
    let ra, rb = find a, find b in
    if ra <> rb then Hashtbl.replace parent ra rb
end

(* ------------------------------------------------------------------ *)
(* Occurs-check to avoid e.g. 'a0 = list['a0]                         *)
(* ------------------------------------------------------------------ *)
let rec occurs id = function
  | TVar v          -> UF.find v = id
  | TList t         -> occurs id t
  | TDict (k,v)     -> occurs id k || occurs id v
  | TFunc (ps,r)    -> List.exists (occurs id) ps || occurs id r
  | _               -> false

(* ------------------------------------------------------------------ *)
(* Unification                                                        *)
(* ------------------------------------------------------------------ *)
let rec unify (s : subst) (t1 : ty) (t2 : ty) : subst =
  match t1, t2 with
  | _ when t1 = t2 ->
      (* identical types unify trivially *)
      s

  | TVar v, t | t, TVar v ->
      let root = UF.find v in
      (match IntMap.find_opt root s with
       | Some bound ->
           (* already mapped; must agree *)
           unify s bound t
       | None ->
           if occurs root t then
             raise (Unify_error (t1, t2));
           IntMap.add root t s)

  | TList a, TList b ->
      unify s a b

  | TDict (k1,v1), TDict (k2,v2) ->
      let s1 = unify s k1 k2 in
      unify s1 v1 v2

  | TFunc (ps1,r1), TFunc (ps2,r2) ->
      if List.length ps1 <> List.length ps2 then
        raise (Unify_error (t1, t2))
      else
        let s1 = List.fold_left2 unify s ps1 ps2 in
        unify s1 r1 r2

  | TAny, _ | _, TAny ->
      (* Any is a wildcard that unifies with anything *)
      s

  | _ ->
      (* all other mismatched cases fail *)
      raise (Unify_error (t1, t2))

(* ------------------------------------------------------------------ *)
(* Apply a substitution recursively                                    *)
(* ------------------------------------------------------------------ *)
let rec apply (s : subst) = function
  | TVar v ->
      let root = UF.find v in
      (match IntMap.find_opt root s with
       | None   -> TVar root
       | Some t -> apply s t)
  | TList t         -> TList (apply s t)
  | TDict (k,v)     -> TDict (apply s k, apply s v)
  | TFunc (ps,ret)  -> TFunc (List.map (apply s) ps, apply s ret)
  | t               -> t
