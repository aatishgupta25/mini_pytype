(** Type representation — the heart of the checker.

    Design notes
    ------------
    * We model *just enough* to cover the MVP subset:
      primitive scalars, homogeneous list/dict, functions, classes,
      and a type variable for inference.
    * Gradual typing needs a top-like `Any`.
    * Everything stays immutable → easier substitution reasoning.
*)

type ty =
  | TInt
  | TFloat
  | TBool
  | TStr
  | TNone
  | TList of ty
  | TDict of ty * ty          (* key, value *)
  | TFunc of ty list * ty     (* params, return *)
  | TObject of string         (* nominal class name *)
  | TVar of int               (* α, β, γ … generated during inference *)
  | TAny

(* Pretty printer used for diagnostics *)
let rec pp fmt = function
  | TInt -> Format.fprintf fmt "int"
  | TFloat -> Format.fprintf fmt "float"
  | TBool -> Format.fprintf fmt "bool"
  | TStr -> Format.fprintf fmt "str"
  | TNone -> Format.fprintf fmt "None"
  | TList t -> Format.fprintf fmt "list[%a]" pp t
  | TDict (k,v) -> Format.fprintf fmt "dict[%a,%a]" pp k pp v
  | TFunc (args,r) ->
      Format.fprintf fmt "Callable[[%a], %a]"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") pp) args
        pp r
  | TObject c -> Format.fprintf fmt "%s" c
  | TVar id -> Format.fprintf fmt "'%c" (Char.chr (97 + (id mod 26)))  (* a,b,c, … *)
  | TAny -> Format.fprintf fmt "Any"
