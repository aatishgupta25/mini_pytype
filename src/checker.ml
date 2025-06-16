(** checker.ml
    Glue: generate constraints → unify → diagnostics list.
*)

open Ast                   (* brings in ty, show_ty, module_ *)
open Constraint_gen       (* gen_module: module_ -> (Ast.ty * Ast.ty) list *)
open Unify                (* unify: subst -> Ast.ty -> Ast.ty -> subst
                              apply: subst -> Ast.ty -> Ast.ty
                              exception Unify_error of Ast.ty * Ast.ty *)

(* Use the same IntMap from Unify for substitutions *)
module Subst = IntMap      (* now Subst.t = Ast.ty IntMap.t *)

type diag =
  | Equate_fail of Ast.ty * Ast.ty

let pp_diag fmt = function
  | Equate_fail (u, v) ->
      Format.fprintf fmt
        "❌  cannot unify %s with %s"
        (show_ty u) (show_ty v)

let check (m : Ast.module_) : diag list =
  (* 1. Generate all (Ast.ty * Ast.ty) constraints *)
  let constrs : (Ast.ty * Ast.ty) list = gen_module m in

  (* 2. Solve them, accumulating any failures *)
  let rec solve (subst : Ast.ty Subst.t) (diags : diag list) = function
    | [] -> List.rev diags
    | (a, b) :: rest ->
      try
        let subst' = unify subst a b in
        solve subst' diags rest
      with Unify_error (u, v) ->
        (* apply existing substitutions before reporting *)
        let u' = apply subst u in
        let v' = apply subst v in
        solve subst (Equate_fail (u', v') :: diags) rest
  in

  (* 3. Start with the empty substitution from Unify.IntMap *)
  solve Subst.empty [] constrs
