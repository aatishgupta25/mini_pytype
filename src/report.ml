open Checker

let ansi_red   s = "\027[31m" ^ s ^ "\027[0m"
let ansi_green s = "\027[32m" ^ s ^ "\027[0m"

let to_pretty (Equate_fail (u,v)) =
  Printf.sprintf "error: Argument type mismatch: expected %s but got %s"
    (Ast.show_ty v) (Ast.show_ty u)

let print diags =
  if diags = [] then
    print_endline (ansi_green "✅  No type errors")
  else begin
    (* first, our custom pretty messages *)
    List.iter (fun d ->
      match d with
      | Equate_fail (u,v) ->
          print_endline (to_pretty (Equate_fail (u,v)))
    ) diags;
    (* then the generic unify diag for extra detail *)
    List.iter (fun d ->
      Format.printf "%s\n" (Format.asprintf "%a" pp_diag d)
    ) diags;
    (* finally the summary *)
    print_endline (ansi_red (Printf.sprintf "%d error(s)" (List.length diags)))
  end

let to_json diags =
  `List (List.map (function
    | Equate_fail (a, b) ->
        `Assoc
          [ "kind", `String "unify"
          ; "msg" , `String
              (Printf.sprintf
                 "cannot unify %s with %s"
                 (Ast.show_ty a) (Ast.show_ty b))
          ]
  ) diags)
