(** json_decode.ml
    Decode JSON (from py2json.py) into the compact IR in [Ast].
*)

open Yojson.Safe.Util
open Ast

exception Decode_error of string

(* ───── Helper shorthands ────────────────────────────────────────────── *)
let str  = to_string
let list = to_list
let mem (j : Yojson.Safe.t) (k : string) : Yojson.Safe.t = member k j
let tag j = mem j "_type" |> str

(* ───── Mini type translator (JSON → Ast.ty) ─────────────────────────── *)
let rec ty_of_json (j : Yojson.Safe.t) : Ast.ty =
  match tag j with
  | "Name" ->
      (match str @@ mem j "id" with
       | "int"   -> TInt
       | "str"   -> TStr
       | "bool"  -> TBool
       | "float" -> TFloat
       | _       -> TAny)
  | "Subscript" ->
      let base  = mem j "value" in
      let slice = mem j "slice" in
      (match tag base, str @@ mem base "id" with
       | "Name", "list" -> TList (ty_of_json slice)
       | "Name", "dict" ->
           (* dict[key_type, val_type] *)
           (match ty_of_json slice with
            | TFunc _ | TVar _ | TAny | TInt | TStr | TBool | TFloat | TList _ ->
                TAny
            | TDict _ ->
                (* nested dicts not supported *)
                TAny)
       | _ -> TAny)
  | _ -> TAny

(* ───── Expression decoder ───────────────────────────────────────────── *)
let rec of_expr j : expr =
  (* If the JSON node is literally null, treat it as a no-op literal *)
  match j with
  | `Null -> ELitStr "None"
  | _ ->
    match tag j with
    | "Constant" ->
        (match mem j "value" with
         | `Int i    -> ELitInt i
         | `Float f  -> ELitFloat f
         | `Bool b   -> ELitBool b
         | `String s -> ELitStr  s
         | `Null     -> ELitStr "None"
         | _         -> ELitStr "UnsupportedConstant")
    | "Name"       -> EName     (str @@ mem j "id")
    | "List"       -> EList     (list (mem j "elts") |> List.map of_expr)
    | "Tuple"      -> EList     (list (mem j "elts") |> List.map of_expr)
    | "Lambda"     -> EName     "<lambda>"        (* skip lambdas *)
    | "ListComp"   -> EList     []                (* skip comprehensions *)
    | "DictComp"   -> EDict     []                (* skip dict comprehensions *)
    | "Dict"       ->
        let keys = list (mem j "keys")   |> List.map of_expr in
        let vals = list (mem j "values") |> List.map of_expr in
        EDict (List.combine keys vals)
    | "Call"       ->
        let fn   = of_expr (mem j "func") in
        let args = list (mem j "args") |> List.map of_expr in
        ECall (fn, args)
    | "BinOp"      ->
        let op = match tag (mem j "op") with
          | "Add"  -> "+" | "Sub"  -> "-" | "Mult" -> "*" | "Div" -> "/"
          | other  -> other
        in
        EBinOp (op, of_expr (mem j "left"), of_expr (mem j "right"))
    | "Attribute"  -> EAttr     (of_expr (mem j "value"), str @@ mem j "attr")
    | "Subscript"  ->
        let value = of_expr (mem j "value") in
        let slice = of_expr (mem j "slice") in
        ESubscript (value, slice)
    | other        -> raise (Decode_error ("expr: unhandled " ^ other))


(* ───── Statement decoder ────────────────────────────────────────────── *)
let rec of_stmt j : stmt =
  match tag j with
  | "If"           -> SExpr (ELitStr "skip")
  | "Import"       -> SExpr (ELitStr "skip")
  | "ImportFrom"   -> SExpr (ELitStr "skip")
  | "Pass"         -> SExpr (ELitStr "skip")
  | "For" | "While"-> SExpr (ELitStr "skip")
  | "Try" | "With" ->  SExpr (ELitStr "skip")
  | "Expr" -> SExpr (of_expr @@ mem j "value")
  | "Assign" ->
      let targets = list (mem j "targets") |> List.map of_expr in
      SAssign (targets, of_expr @@ mem j "value")
  | "AnnAssign" ->
      (* annotated assignment: ignore the annotation itself *)
      let target = of_expr (mem j "target") in
      let value  = of_expr (mem j "value") in
      SAssign ([target], value)
  | "Return" ->
      let v = mem j "value" in
      SReturn (if v = `Null then None else Some (of_expr v))
  | "FunctionDef" ->
      SFuncDef (of_func j)
  | "ClassDef" ->
      SClassDef (of_class j)
  | other ->
      raise (Decode_error ("stmt: unhandled " ^ other))

and of_func j : func_def =
  let param_list =
    list (mem (mem j "args") "args")
    |> List.map (fun p ->
         let name = str @@ mem p "arg" in
         let ann  = mem p "annotation" in
         let ty_opt =
           if ann = `Null then None else Some (ty_of_json ann)
         in
         (match ty_opt with
          | Some ty -> Printf.printf "[DEBUG] param %s : %s\n%!" name (Ast.show_ty ty)
          | None    -> ());
         (name, ty_opt))
  in
  let ret_ann = mem j "returns" in
  let ret_ty_opt =
    if ret_ann = `Null then None else Some (ty_of_json ret_ann)
  in
  (match ret_ty_opt with
   | Some ty -> Printf.printf "[DEBUG] return : %s\n%!" (Ast.show_ty ty)
   | None    -> ());
  { fname   = str @@ mem j "name"
  ; params  = param_list
  ; returns = ret_ty_opt
  ; body    = list (mem j "body") |> List.map of_stmt
  }

and of_class j : class_def =
  let bases = list (mem j "bases") in
  let base =
    match bases with
    | [] -> None
    | [b] when tag b = "Name" -> Some (str @@ mem b "id")
    | _  -> None
  in
  let methods =
    list (mem j "body")
    |> List.filter (fun n -> tag n = "FunctionDef")
    |> List.map of_func
  in
  { cname   = str @@ mem j "name"
  ; base
  ; methods }

(* ───── Module decoder (entry) ────────────────────────────────────────── *)
let of_module j : module_ =
  list (mem j "body") |> List.map of_stmt
