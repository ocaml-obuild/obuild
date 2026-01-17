open Lib
open Test_framework
open Printf

(** Test helpers for parser testing *)

(** {1 META Parser Helpers} *)

let parse_meta_string content name =
  Meta.parse (Filepath.fp name) content name

let assert_meta_parses ~content ~name =
  try
    let _ = parse_meta_string content name in
    Success
  with exn ->
    Failure (sprintf "META parsing failed: %s\nInput:\n%s"
             (Printexc.to_string exn) content)

let assert_meta_parse_error ~content ~expected_msg ~name =
  try
    let _ = parse_meta_string content name in
    Failure (sprintf "Expected META parse error, but parsing succeeded.\nInput:\n%s" content)
  with
  | Meta.MetaParseError (_, msg) ->
      assert_string_contains ~haystack:msg ~needle:expected_msg ~name
  | exn ->
      Failure (sprintf "Expected MetaParseError containing '%s', got: %s"
               expected_msg (Printexc.to_string exn))

let assert_meta_field ~content ~pkg_name ~field_name ~expected_value ~test_name =
  try
    let pkg = parse_meta_string content pkg_name in
    let actual = match field_name with
      | "version" -> pkg.Meta.Pkg.version
      | "description" -> pkg.Meta.Pkg.description
      | "directory" -> pkg.Meta.Pkg.directory
      | _ -> failwith ("Unknown field: " ^ field_name)
    in
    assert_equal ~expected:expected_value ~actual ~name:test_name
  with exn ->
    Failure (sprintf "Failed to get field '%s': %s" field_name (Printexc.to_string exn))

(** {1 Expression Parser Helpers} *)

let parse_expr_string name expr_str =
  Expr.parse name expr_str

let assert_expr_parses ~content ~name =
  try
    let _ = parse_expr_string name content in
    Success
  with exn ->
    Failure (sprintf "Expression parsing failed: %s\nInput: %s"
             (Printexc.to_string exn) content)

let assert_expr_parse_error ~content ~expected_msg ~name =
  try
    let _ = parse_expr_string name content in
    Failure (sprintf "Expected expression parse error, but parsing succeeded.\nInput: %s" content)
  with
  | Expr.CannotParseConstraints (_, msg) ->
      assert_string_contains ~haystack:msg ~needle:expected_msg ~name
  | exn ->
      Failure (sprintf "Expected CannotParseConstraints containing '%s', got: %s"
               expected_msg (Printexc.to_string exn))

let assert_expr_eval ~expr ~version ~expected ~name =
  match expr with
  | None ->
      if expected then Success
      else Failure "Expression is None but expected to evaluate to false"
  | Some e ->
      let actual = Expr.eval version e in
      if actual = expected then Success
      else Failure (sprintf "Expected %b, got %b for version %s" expected actual version)

(** {1 Project Parser Helpers} *)

(* Note: Project parser needs special handling because it reads from files.
   We'll create temporary files for testing. *)

let with_temp_project_file content test_func =
  let temp_file = Filename.temp_file "test_project" ".obuild" in
  try
    let oc = open_out temp_file in
    output_string oc content;
    close_out oc;
    let old_dir = Sys.getcwd () in
    let temp_dir = Filename.dirname temp_file in
    Sys.chdir temp_dir;
    let result = test_func () in
    Sys.chdir old_dir;
    Sys.remove temp_file;
    result
  with exn ->
    (try Sys.remove temp_file with _ -> ());
    raise exn

let assert_project_parses ~content ~name =
  try
    with_temp_project_file content (fun () ->
      let _ = Project_read.read () in
      Success)
  with exn ->
    Failure (sprintf "Project parsing failed: %s\nInput:\n%s"
             (Printexc.to_string exn) content)

let assert_project_parse_error ~content ~expected_msg ~name =
  try
    with_temp_project_file content (fun () ->
      let _ = Project_read.read () in
      Failure (sprintf "Expected project parse error, but parsing succeeded.\nInput:\n%s" content))
  with
  | Project.MissingField field ->
      assert_string_contains ~haystack:("Missing field: " ^ field) ~needle:expected_msg ~name
  | Project.InvalidConfFile msg ->
      assert_string_contains ~haystack:msg ~needle:expected_msg ~name
  | Project.BlockSectionAsValue field ->
      assert_string_contains ~haystack:("Block section as value: " ^ field) ~needle:expected_msg ~name
  | exn ->
      let msg = Printexc.to_string exn in
      assert_string_contains ~haystack:msg ~needle:expected_msg ~name

(** {1 Libname Helpers} *)

let assert_libname_parse ~input ~expected_main ~expected_subs ~name =
  try
    let libname = Libname.of_string input in
    let main_ok = libname.Libname.main_name = expected_main in
    let subs_ok = libname.Libname.subnames = expected_subs in
    if main_ok && subs_ok then Success
    else
      Failure (sprintf "Libname parse mismatch.\nExpected: %s.%s\nGot: %s.%s"
               expected_main (String.concat "." expected_subs)
               libname.Libname.main_name (String.concat "." libname.Libname.subnames))
  with exn ->
    Failure (sprintf "Libname parsing failed: %s" (Printexc.to_string exn))

(** {1 Common Test Data} *)

(** Minimal valid META file *)
let minimal_meta = {|
version = "1.0.0"
description = "Test package"
|}

(** Minimal valid .obuild file *)
let minimal_project = {|
name: test
version: 1.0.0
obuild-ver: 1
|}

(** Complete example META file *)
let example_meta = {|
version = "2.0.0"
description = "Example package with all features"
requires = "unix, str"
directory = "^"
archive(byte) = "example.cma"
archive(native) = "example.cmxa"
archive(byte,mt) = "example_mt.cma"
package "sub" (
  description = "Subpackage"
  archive(byte) = "sub.cma"
)
|}

(** Complete example .obuild file *)
let example_project = {|
name = example
version = 2.0.0
obuild-ver = 1
synopsis = "An example project"
description = "This is a complete example project"

library mylib
  modules: Foo, Bar
  build-deps: unix

executable myexe
  main-is: main.ml
  build-deps: mylib
|}
