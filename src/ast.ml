(** ast.ml
    Definition of the compact IR for Python programs (types, expressions, statements, functions, classes, modules).
*)

(* ───── Type representations ────────────────────────────────────────────────── *)
type ty =
  | TInt
  | TStr
  | TBool
  | TFloat
  | TList of ty
  | TDict of ty * ty       (* key and value types *)
  | TFunc of ty list * ty  (* parameter types and return type *)
  | TVar of int            (* type variable for inference *)
  | TAny                   (* fallback for unsupported or unrecognized annotations *)

type location = { line: int; column: int }

let rec show_ty = function
  | TInt              -> "int"
  | TStr              -> "str"
  | TBool             -> "bool"
  | TFloat            -> "float"
  | TList t           -> "list[" ^ show_ty t ^ "]"
  | TDict (k, v)      -> "dict[" ^ show_ty k ^ ", " ^ show_ty v ^ "]"
  | TFunc (params, r) ->
      let ps = String.concat ", " (List.map show_ty params) in
      Printf.sprintf "(%s) -> %s" ps (show_ty r)
  | TVar id           -> Printf.sprintf "'a%d" id
  | TAny              -> "Any"

(* ───── Expression AST ────────────────────────────────────────────────────── *)
type expr =
  | ELitInt    of int
  | ELitFloat  of float
  | ELitBool   of bool
  | ELitStr    of string
  | EName      of string
  | EList      of expr list
  | EDict      of (expr * expr) list
  | ECall      of expr * expr list
  | EBinOp     of string * expr * expr
  | EAttr      of expr * string
  | ESubscript of expr * expr

(* ───── Statement AST ──────────────────────────────────────────────────────── *)
type stmt =
  | SExpr     of expr
  | SAssign   of expr list * expr
  | SReturn   of expr option
  | SFuncDef  of func_def
  | SClassDef of class_def

(* ───── Function AST ───────────────────────────────────────────────────────── *)
and func_def = {
  fname   : string;
  params  : (string * ty option) list;
  returns : ty option;
  body    : stmt list;
}

(* ───── Class AST ──────────────────────────────────────────────────────────── *)
and class_def = {
  cname   : string;
  base    : string option;
  methods : func_def list;
}

(* ───── Module (top-level) ────────────────────────────────────────────────── *)
type module_ = stmt list
