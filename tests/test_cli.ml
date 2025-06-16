(* tests/alcotest.ml *)
open Alcotest
open Yojson.Safe

module Json_decode = Mini_pytype_lib.Json_decode
module Checker = Mini_pytype_lib.Checker

(* ──────────────────────────────────────────────────────────────── *)
(* Locate the project root by looking for py/py2json.py          *)
(* ──────────────────────────────────────────────────────────────── *)
let rec find_root dir =
  let candidate = Filename.concat dir "py/py2json.py" in
  if Sys.file_exists candidate then dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then failwith "project root not found"
    else find_root parent

let root_dir =
  Sys.executable_name
  |> Filename.dirname
  |> find_root

let helper_py = Filename.concat root_dir "py/py2json.py"
let data_dir  = Filename.concat root_dir "tests/data"

(* ──────────────────────────────────────────────────────────────── *)
(* Run the checker on a given file, return list of diagnostics     *)
(* ──────────────────────────────────────────────────────────────── *)
let run file =
  let cmd = Printf.sprintf "python3 %s %s" helper_py file in
  let ic  = Unix.open_process_in cmd in
  let json = from_channel ic in
  ignore (Unix.close_process_in ic);
  let ast = Json_decode.of_module json in
  Checker.check ast

(* ──────────────────────────────────────────────────────────────── *)
(* Helpers for good / bad tests                                   *)
(* ──────────────────────────────────────────────────────────────── *)
let check_ok file () =
  match run file with
  | [] -> ()
  | ds -> failf "%s should pass but got %d diagnostic(s)" file (List.length ds)

let check_err file () =
  match run file with
  | [] -> failf "%s should fail but produced 0 diagnostic(s)" file
  | _  -> ()

(* ──────────────────────────────────────────────────────────────── *)
(* Discover all .py files under tests/data and categorize them    *)
(* ──────────────────────────────────────────────────────────────── *)
let discover_tests () =
  Sys.readdir data_dir
  |> Array.to_list
  |> List.filter (fun fname -> Filename.check_suffix fname ".py")
  |> List.map (fun fname ->
       let full = Filename.concat data_dir fname in
       let case =
         if String.starts_with ~prefix:"good_" fname then `Good
         else if String.starts_with ~prefix:"bad_"  fname then `Bad
         else `Skip
       in
       (fname, full, case)
     )

(* ──────────────────────────────────────────────────────────────── *)
(* Build Alcotest test cases from discovered files               *)
(* ──────────────────────────────────────────────────────────────── *)
module M = Map.Make(String)

let tests_by_kind =
  let cases = discover_tests () in
  List.fold_left
    (fun acc (fname, full, case) ->
       let suite_name = match case with `Good -> "positive" | `Bad -> "negative" | `Skip -> "smoke" in
       let testfn = match case with
         | `Good -> test_case fname `Quick (check_ok full)
         | `Bad  -> test_case fname `Quick (check_err full)
         | `Skip -> test_case fname `Quick (fun () -> ignore (run full))
       in
       M.update suite_name
         (function None -> Some [testfn] | Some l -> Some (testfn :: l))
         acc)
    M.empty
    cases

let () =
  let suites =
    tests_by_kind
    |> M.bindings
    |> List.map (fun (suite_name, tests) -> (suite_name, tests))
  in
  Alcotest.run "mini-pytype" suites
