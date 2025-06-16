(* src/cli.ml *)

open Printf
(* open Yojson.Safe *)
open Mini_pytype_lib

(* ---- Your modules *)
module J  = Json_decode    (* of_module : Yojson.Safe.t -> Ast.module_ *)
module CG = Constraint_gen (* gen_module : Ast.module_ -> (ty * ty) list *)
module C  = Checker        (* check      : Ast.module_ -> diag list *)
module R  = Report         (* print      : diag list -> unit *)

(* ---- Flags *)
let verbose_constraints = ref false
let helper_py            = ref "py/py2json.py"

let usage = "Usage: mini-pytype [--verbose-constraints] [--helper-py PATH] file1.py [file2.py ...]"

let specs = [
  ("--verbose-constraints",
    Arg.Set verbose_constraints,
    "  after parsing, print total constraint count");
  ("--helper-py",
    Arg.Set_string helper_py,
    "  path to py2json.py (default: py/py2json.py)");
]

let files = ref []

let anon_fun fn =
  files := !files @ [fn]

let main () =
  Arg.parse specs anon_fun usage;
  if !files = [] then begin
    eprintf "%s\n" usage;
    exit 1
  end;
  (* Process each file in turn *)
  List.iter
    (fun path ->
      (* 1. Shell out to py2json.py to get JSON AST *)
      let cmd = sprintf "python3 %s %s" !helper_py path in
      let ic  = Unix.open_process_in cmd in
      let json =
        try Yojson.Safe.from_channel ic
        with exn ->
          eprintf "Failed to parse JSON from %s: %s\n%!" path (Printexc.to_string exn);
          exit 1
      in
      ignore (Unix.close_process_in ic);

      (* 2. Decode into your IR *)
      let ast =
        try J.of_module json
        with J.Decode_error msg ->
          eprintf "JSON decode error in %s: %s\n%!" path msg;
          exit 1
      in

      (* 3. Optionally compute & print constraint count *)
      if !verbose_constraints then begin
        let constrs = CG.gen_module ast in
        printf "Constraints: %d\n%!" (List.length constrs)
      end;

      (* 4. Run the checker *)
      let diags = C.check ast in

      (* 5. Print diagnostics *)
      R.print diags
    (* ... all your existing logic ... *)
    )
  !files

(* let () =
  Arg.parse specs anon_fun usage;
  if !files = [] then begin
    eprintf "%s\n" usage;
    exit 1
  end;

  (* Process each file in turn *)
  List.iter
    (fun path ->
      (* 1. Shell out to py2json.py to get JSON AST *)
      let cmd = sprintf "python3 %s %s" !helper_py path in
      let ic  = Unix.open_process_in cmd in
      let json =
        try Yojson.Safe.from_channel ic
        with exn ->
          eprintf "Failed to parse JSON from %s: %s\n%!" path (Printexc.to_string exn);
          exit 1
      in
      ignore (Unix.close_process_in ic);

      (* 2. Decode into your IR *)
      let ast =
        try J.of_module json
        with J.Decode_error msg ->
          eprintf "JSON decode error in %s: %s\n%!" path msg;
          exit 1
      in

      (* 3. Optionally compute & print constraint count *)
      if !verbose_constraints then begin
        let constrs = CG.gen_module ast in
        printf "Constraints: %d\n%!" (List.length constrs)
      end;

      (* 4. Run the checker *)
      let diags = C.check ast in

      (* 5. Print diagnostics *)
      R.print diags
    )
    !files *)
