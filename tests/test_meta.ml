open Lib
open Test_framework
open Printf

(* Helper functions *)

let parse_meta_string content name = Meta.parse (Filepath.fp name) content name

let archive_to_string (preds, archive) =
  let pred_strs = List.map Meta.Predicate.to_string preds in
  sprintf "archive(%s) = %s" (String.concat "," pred_strs) archive

let archives_to_string archives = String.concat "; " (List.map archive_to_string archives)

(* Compat helpers for old OCaml *)
let list_find_opt pred lst =
  try Some (List.find pred lst) with Not_found -> None

(* Easy test cases *)

let test_basic_meta () =
  let content = "\
version = \"1.0.0\"\n\
description = \"A simple package\"\n\
requires = \"unix\"\n\
archive(byte) = \"simple.cma\"\n\
archive(native) = \"simple.cmxa\"\n"
  in
  let pkg = parse_meta_string content "simple" in
  let tests =
    [
      assert_equal ~expected:"1.0.0" ~actual:pkg.Meta.Pkg.version ~name:"version";
      assert_equal ~expected:"A simple package" ~actual:pkg.Meta.Pkg.description ~name:"description";
    ]
  in
  match
    list_find_opt
      (function
        | TestFailure _ -> true
        | Success -> false)
      tests
  with
  | Some (TestFailure msg) -> TestFailure msg
  | _ -> Success

let test_empty_fields () =
  let content = "\
version = \"\"\n\
description = \"\"\n\
requires = \"\"\n" in
  let pkg = parse_meta_string content "empty" in
  assert_equal ~expected:"" ~actual:pkg.Meta.Pkg.version ~name:"empty version"

let test_simple_archive () =
  let content = "\
archive(byte) = \"test.cma\"\n\
archive(native) = \"test.cmxa\"\n" in
  let pkg = parse_meta_string content "test" in
  let has_byte =
    List.exists (fun (preds, _) -> List.mem Meta.Predicate.Byte preds) pkg.Meta.Pkg.archives
  in
  let has_native =
    List.exists (fun (preds, _) -> List.mem Meta.Predicate.Native preds) pkg.Meta.Pkg.archives
  in
  if has_byte && has_native then
    Success
  else
    TestFailure "Missing byte or native archives"

let test_basic_package () =
  let content = "\
version = \"1.0\"\n\
package \"sub\" (\n\
  description = \"Subpackage\"\n\
  archive(byte) = \"sub.cma\"\n\
)\n"
  in
  let pkg = parse_meta_string content "parent" in
  match pkg.Meta.Pkg.subs with
  | [ sub ] ->
      assert_equal ~expected:"Subpackage" ~actual:sub.Meta.Pkg.description
        ~name:"subpackage description"
  | _ -> TestFailure "Expected exactly one subpackage"

(* Difficult test cases *)

let test_multiline_values () =
  let content = "\
requires =\n\
\"unix\n\
 str  \n\
 bigarray\"\n\
description = \"Multi\n\
line\n\
description\"\n" in
  try
    let _pkg = parse_meta_string content "multiline" in
    (* If parsing succeeds, check if requires were parsed *)
    Success
  with
  | Meta.MetaParseError _ ->
      TestFailure "Failed to parse multiline values (expected to fail with current parser)"
  | _ -> TestFailure "Unexpected error parsing multiline values"

let test_negated_predicates () =
  let content = "\
requires(-mt) = \"single_threaded_lib\"\n\
requires(mt) = \"threaded_lib\"\n\
archive(byte,-debug) = \"release.cma\"\n\
archive(byte,debug) = \"debug.cma\"\n"
  in
  try
    let _pkg = parse_meta_string content "negated" in
    Success
  with
  | Meta.MetaParseError (_, msg) -> TestFailure ("Negated predicates failed: " ^ msg)
  | exn -> TestFailure ("Unexpected error: " ^ Printexc.to_string exn)

let test_unknown_fields () =
  let content = "\
version = \"1.0\"\n\
library_kind = \"ppx_rewriter\"\n\
custom_field = \"custom_value\"\n\
xen_linkopts = \"-lxen\"\n\
browse_interfaces = \"Unit name: Test\"\n"
  in
  try
    let pkg = parse_meta_string content "unknown" in
    (* Check that unknown fields were stored in assignments *)
    let has_library_kind =
      List.exists (fun (field, _) -> field = "library_kind") pkg.Meta.Pkg.assignment
    in
    let has_custom_field =
      List.exists (fun (field, _) -> field = "custom_field") pkg.Meta.Pkg.assignment
    in
    if has_library_kind || has_custom_field then
      Success
    else
      TestFailure "Unknown fields not stored in assignments"
  with
  | Meta.MetaParseError (_, msg) -> TestFailure ("Unknown fields failed: " ^ msg)
  | exn -> TestFailure ("Unexpected error: " ^ Printexc.to_string exn)

let test_complex_predicates () =
  let content = "\
archive(byte,mt,mt_posix) = \"threads_posix.cma\"\n\
archive(native,mt,mt_vm) = \"threads_vm.cmxa\"\n\
requires(ppx_driver,byte) = \"ppx_lib\"\n\
warning(-mt,-debug) = \"Missing thread support\"\n"
  in
  try
    let _pkg = parse_meta_string content "complex" in
    Success
  with
  | Meta.MetaParseError (_, msg) -> TestFailure ("Complex predicates failed: " ^ msg)
  | exn -> TestFailure ("Unexpected error: " ^ Printexc.to_string exn)

let test_plugin_syntax () =
  let content = "\
plugin(byte) = \"test.cma\"\n\
plugin(native) = \"test.cmxs\"\n\
archive(byte,plugin) = \"test_plugin.cma\"\n"
  in
  try
    let _pkg = parse_meta_string content "plugin" in
    Success
  with
  | Meta.MetaParseError (_, msg) -> TestFailure ("Plugin syntax failed: " ^ msg)
  | exn -> TestFailure ("Unexpected error: " ^ Printexc.to_string exn)

let test_ppx_syntax () =
  let content = "\
ppx(-ppx_driver,-custom_ppx) = \"./ppx.exe --as-ppx\"\n\
ppx(ppx_driver) = \"ppx_driver.exe\"\n\
ppxopt(-ppx_driver) = \"-package deriving\"\n"
  in
  try
    let _pkg = parse_meta_string content "ppx" in
    Success
  with
  | Meta.MetaParseError (_, msg) -> TestFailure ("PPX syntax failed: " ^ msg)
  | exn -> TestFailure ("Unexpected error: " ^ Printexc.to_string exn)

let test_nested_packages () =
  let content = "\
version = \"1.0\"\n\
package \"level1\" (\n\
  version = \"1.1\"\n\
  package \"level2\" (\n\
    version = \"1.2\"\n\
    archive(byte) = \"deep.cma\"\n\
    package \"level3\" (\n\
      description = \"Deep nesting\"\n\
    )\n\
  )\n\
)\n"
  in
  try
    let pkg = parse_meta_string content "nested" in
    (* Navigate to level3 package *)
    match pkg.Meta.Pkg.subs with
    | [ level1 ] -> (
        match level1.Meta.Pkg.subs with
        | [ level2 ] -> (
            match level2.Meta.Pkg.subs with
            | [ level3 ] ->
                assert_equal ~expected:"Deep nesting" ~actual:level3.Meta.Pkg.description
                  ~name:"nested package"
            | _ -> TestFailure "Expected level3 package")
        | _ -> TestFailure "Expected level2 package")
    | _ -> TestFailure "Expected level1 package"
  with
  | Meta.MetaParseError (_, msg) -> TestFailure ("Nested packages failed: " ^ msg)
  | exn -> TestFailure ("Unexpected error: " ^ Printexc.to_string exn)

(* Edge cases that should break current parser *)

let test_append_operator () =
  let content = "\
archive(byte) = \"base.cma\"\n\
archive(byte) += \"extra.cma\"\n\
requires = \"unix\"\n\
requires += \"str\"\n"
  in
  try
    let pkg = parse_meta_string content "append" in
    (* Check if append archives were created *)
    let has_append = List.length pkg.Meta.Pkg.append_archives > 0 in
    if has_append then
      Success
    else
      TestFailure "Append operator not working correctly"
  with
  | Meta.MetaParseError (_, msg) -> TestFailure ("Append operator failed: " ^ msg)
  | exn -> TestFailure ("Unexpected error: " ^ Printexc.to_string exn)

let test_comments_and_whitespace () =
  let content = "\
# This is a comment\n\
version = \"1.0\"   # End of line comment\n\
\n\
# Empty lines and spacing\n\
description = \"Test\"\n\
\n\
    # Indented comment\n\
archive(byte) = \"test.cma\"\n"
  in
  try
    let pkg = parse_meta_string content "comments" in
    assert_equal ~expected:"1.0" ~actual:pkg.Meta.Pkg.version ~name:"version with comments"
  with
  | Meta.MetaParseError (_, msg) -> TestFailure ("Comments failed: " ^ msg)
  | exn -> TestFailure ("Unexpected error: " ^ Printexc.to_string exn)

let test_real_world_ppxlib () =
  (* Based on actual ppxlib META file *)
  let content = "\
version = \"0.36.0\"\n\
description = \"\"\n\
requires =\n\
\"compiler-libs.common\n\
 ocaml-compiler-libs.shadow\n\
 ppx_derivers\"\n\
archive(byte) = \"ppxlib.cma\"\n\
archive(native) = \"ppxlib.cmxa\"\n\
plugin(byte) = \"ppxlib.cma\"\n\
plugin(native) = \"ppxlib.cmxs\"\n"
  in
  try
    let _pkg = parse_meta_string content "ppxlib" in
    Success
  with
  | Meta.MetaParseError (_, msg) -> TestFailure ("Real ppxlib META failed: " ^ msg)
  | exn -> TestFailure ("Unexpected error: " ^ Printexc.to_string exn)

let test_malformed_syntax () =
  let content = "\
version = \"1.0\"\n\
broken_field_no_value =\n\
archive(byte) =\n\
requires = unix str\n"
  in
  (* This should fail to parse *)
  try
    let _ = parse_meta_string content "malformed" in
    TestFailure "Expected malformed syntax to fail, but it succeeded"
  with
  | Meta.MetaParseError _ -> Success
  | exn -> TestFailure ("Expected MetaParseError, got: " ^ Printexc.to_string exn)

let test_libname_parsing () =
  let libname_str = "ppx_stable_witness.stable_witness" in
  let libname = Libname.of_string libname_str in
  printf "Testing libname: %s\n" libname_str;
  printf "  main_name: %s\n" libname.Libname.main_name;
  printf "  subnames: [%s]\n" (String.concat "; " libname.Libname.subnames);
  printf "  full string: %s\n" (Libname.to_string libname);
  if
    libname.Libname.main_name = "ppx_stable_witness"
    && libname.Libname.subnames = [ "stable_witness" ]
  then
    Success
  else
    TestFailure "Libname parsing incorrect"

let test_ppx_stable_witness_findlib () =
  FindlibConf.load ();
  try
    let path, pkg = Meta.find_lib "ppx_stable_witness" in
    let stable_witness_libname = Libname.of_string "ppx_stable_witness.stable_witness" in
    let resolved_pkg = Meta.Pkg.find stable_witness_libname.Libname.subnames pkg in
    if resolved_pkg.Meta.Pkg.name = "stable_witness" then
      Success
    else
      TestFailure ("Expected stable_witness package, got: " ^ resolved_pkg.Meta.Pkg.name)
  with
  | Meta.LibraryNotFound name -> TestFailure ("LibraryNotFound: " ^ name)
  | exn -> TestFailure ("Unexpected error: " ^ Printexc.to_string exn)

let test_metacache_consistency () =
  FindlibConf.load ();
  try
    (* First populate cache using Metacache.get *)
    let libname = Libname.of_string "ppx_stable_witness.stable_witness" in
    let _, cached_meta = Metacache.get libname.Libname.main_name in

    (* Then try to get subpackage *)
    let cached_pkg = Meta.Pkg.find libname.Libname.subnames cached_meta in

    if cached_pkg.Meta.Pkg.name = "stable_witness" then
      Success
    else
      TestFailure ("Expected stable_witness from cache, got: " ^ cached_pkg.Meta.Pkg.name)
  with
  | Meta.LibraryNotFound name -> TestFailure ("LibraryNotFound in cache test: " ^ name)
  | Dependencies.DependencyMissing name -> TestFailure ("DependencyMissing in cache test: " ^ name)
  | exn -> TestFailure ("Cache test error: " ^ Printexc.to_string exn)

(* Test suite *)

let all_tests =
  [
    (* Easy cases *)
    make_test "basic_meta" test_basic_meta;
    make_test "empty_fields" test_empty_fields;
    make_test "simple_archive" test_simple_archive;
    make_test "basic_package" test_basic_package;
    (* Difficult cases *)
    make_test "multiline_values" test_multiline_values;
    make_test "negated_predicates" test_negated_predicates;
    make_test "unknown_fields" test_unknown_fields;
    make_test "complex_predicates" test_complex_predicates;
    make_test "plugin_syntax" test_plugin_syntax;
    make_test "ppx_syntax" test_ppx_syntax;
    make_test "nested_packages" test_nested_packages;
    (* Edge cases *)
    make_test "append_operator" test_append_operator;
    make_test "comments_and_whitespace" test_comments_and_whitespace;
    make_test "real_world_ppxlib" test_real_world_ppxlib;
    make_test "malformed_syntax" test_malformed_syntax;
    (* Real library resolution *)
    make_test "libname_parsing" test_libname_parsing;
    make_test "ppx_stable_witness_findlib" test_ppx_stable_witness_findlib;
    make_test "metacache_consistency" test_metacache_consistency;
  ]

let () = run_tests all_tests
