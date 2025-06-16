(* src/report.ml *)

open Checker
open Ast

let ansi_red   s = "\027[31m" ^ s ^ "\027[0m"
let ansi_green s = "\027[32m" ^ s ^ "\027[0m"

(* Human-friendly single-line message *)
let to_pretty (Equate_fail (got, expected)) =
  Printf.sprintf
    "error: Argument type mismatch: expected %s but got %s"
    (show_ty expected)
    (show_ty got)

(* Print either a success banner or detailed inline diagnostics *)
let print diags =
  if diags = [] then
    print_endline (ansi_green "✅  No type errors")
  else begin
    (* 1) Custom pretty messages *)
    List.iter (fun d ->
      match d with
      | Equate_fail (got, expected) ->
          print_endline (to_pretty (Equate_fail (got, expected)))
    ) diags;
    (* 2) Fallback to generic unify diagnostics for more context *)
    List.iter (fun d ->
      Format.printf "%s\n" (Format.asprintf "%a" pp_diag d)
    ) diags;
    (* 3) Summary *)
    print_endline (ansi_red (Printf.sprintf "%d error(s)" (List.length diags)))
  end

(* Emit JSON array of diagnostics with expected/got fields *)
let to_json diags =
  `List (List.map (function
    | Equate_fail (got, expected) ->
        `Assoc [
          ("kind"    , `String "unify");
          ("expected", `String (show_ty expected));
          ("got"     , `String (show_ty got));
          ("message" , `String
            (Printf.sprintf "cannot unify %s with %s"
               (show_ty expected)
               (show_ty got)))
        ]
  ) diags)
