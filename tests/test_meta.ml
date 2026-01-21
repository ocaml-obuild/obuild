open Lib
open Test_framework
open Printf

(* Helper functions *)

let parse_meta_string content name = Meta.parse (Filepath.fp name) content name

let archive_to_string (preds, archive) =
  let pred_strs = List.map Meta.Predicate.to_string preds in
  sprintf "archive(%s) = %s" (String.concat "," pred_strs) archive

let archives_to_string archives = String.concat "; " (List.map archive_to_string archives)

(* Easy test cases *)

let test_basic_meta () =
  let content =
    {|
version = "1.0.0"
description = "A simple package"
requires = "unix"
archive(byte) = "simple.cma"
archive(native) = "simple.cmxa"
|}
  in
  let pkg = parse_meta_string content "simple" in
  let tests =
    [
      assert_equal ~expected:"1.0.0" ~actual:pkg.Meta.Pkg.version ~name:"version";
      assert_equal ~expected:"A simple package" ~actual:pkg.Meta.Pkg.description ~name:"description";
    ]
  in
  match
    List.find_opt
      (function
        | Failure _ -> true
        | Success -> false)
      tests
  with
  | Some (Failure msg) -> Failure msg
  | _ -> Success

let test_empty_fields () =
  let content = {|
version = ""
description = ""
requires = ""
|} in
  let pkg = parse_meta_string content "empty" in
  assert_equal ~expected:"" ~actual:pkg.Meta.Pkg.version ~name:"empty version"

let test_simple_archive () =
  let content = {|
archive(byte) = "test.cma"
archive(native) = "test.cmxa"  
|} in
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
    Failure "Missing byte or native archives"

let test_basic_package () =
  let content =
    {|
version = "1.0"
package "sub" (
  description = "Subpackage"
  archive(byte) = "sub.cma"
)
|}
  in
  let pkg = parse_meta_string content "parent" in
  match pkg.Meta.Pkg.subs with
  | [ sub ] ->
      assert_equal ~expected:"Subpackage" ~actual:sub.Meta.Pkg.description
        ~name:"subpackage description"
  | _ -> Failure "Expected exactly one subpackage"

(* Difficult test cases *)

let test_multiline_values () =
  let content = {|
requires =
"unix
 str  
 bigarray"
description = "Multi
line
description"
|} in
  try
    let pkg = parse_meta_string content "multiline" in
    (* If parsing succeeds, check if requires were parsed *)
    Success
  with
  | Meta.MetaParseError _ ->
      Failure "Failed to parse multiline values (expected to fail with current parser)"
  | _ -> Failure "Unexpected error parsing multiline values"

let test_negated_predicates () =
  let content =
    {|
requires(-mt) = "single_threaded_lib"
requires(mt) = "threaded_lib"
archive(byte,-debug) = "release.cma"
archive(byte,debug) = "debug.cma"
|}
  in
  try
    let pkg = parse_meta_string content "negated" in
    Success
  with
  | Meta.MetaParseError (_, msg) -> Failure ("Negated predicates failed: " ^ msg)
  | exn -> Failure ("Unexpected error: " ^ Printexc.to_string exn)

let test_unknown_fields () =
  let content =
    {|
version = "1.0"
library_kind = "ppx_rewriter"
custom_field = "custom_value"
xen_linkopts = "-lxen"
browse_interfaces = "Unit name: Test"
|}
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
      Failure "Unknown fields not stored in assignments"
  with
  | Meta.MetaParseError (_, msg) -> Failure ("Unknown fields failed: " ^ msg)
  | exn -> Failure ("Unexpected error: " ^ Printexc.to_string exn)

let test_complex_predicates () =
  let content =
    {|
archive(byte,mt,mt_posix) = "threads_posix.cma"
archive(native,mt,mt_vm) = "threads_vm.cmxa"
requires(ppx_driver,byte) = "ppx_lib"
warning(-mt,-debug) = "Missing thread support"
|}
  in
  try
    let pkg = parse_meta_string content "complex" in
    Success
  with
  | Meta.MetaParseError (_, msg) -> Failure ("Complex predicates failed: " ^ msg)
  | exn -> Failure ("Unexpected error: " ^ Printexc.to_string exn)

let test_plugin_syntax () =
  let content =
    {|
plugin(byte) = "test.cma"
plugin(native) = "test.cmxs"
archive(byte,plugin) = "test_plugin.cma"
|}
  in
  try
    let pkg = parse_meta_string content "plugin" in
    Success
  with
  | Meta.MetaParseError (_, msg) -> Failure ("Plugin syntax failed: " ^ msg)
  | exn -> Failure ("Unexpected error: " ^ Printexc.to_string exn)

let test_ppx_syntax () =
  let content =
    {|
ppx(-ppx_driver,-custom_ppx) = "./ppx.exe --as-ppx"
ppx(ppx_driver) = "ppx_driver.exe"
ppxopt(-ppx_driver) = "-package deriving"
|}
  in
  try
    let pkg = parse_meta_string content "ppx" in
    Success
  with
  | Meta.MetaParseError (_, msg) -> Failure ("PPX syntax failed: " ^ msg)
  | exn -> Failure ("Unexpected error: " ^ Printexc.to_string exn)

let test_nested_packages () =
  let content =
    {|
version = "1.0"
package "level1" (
  version = "1.1"
  package "level2" (
    version = "1.2" 
    archive(byte) = "deep.cma"
    package "level3" (
      description = "Deep nesting"
    )
  )
)
|}
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
            | _ -> Failure "Expected level3 package")
        | _ -> Failure "Expected level2 package")
    | _ -> Failure "Expected level1 package"
  with
  | Meta.MetaParseError (_, msg) -> Failure ("Nested packages failed: " ^ msg)
  | exn -> Failure ("Unexpected error: " ^ Printexc.to_string exn)

(* Edge cases that should break current parser *)

let test_append_operator () =
  let content =
    {|
archive(byte) = "base.cma"
archive(byte) += "extra.cma"
requires = "unix"
requires += "str"
|}
  in
  try
    let pkg = parse_meta_string content "append" in
    (* Check if append archives were created *)
    let has_append = List.length pkg.Meta.Pkg.append_archives > 0 in
    if has_append then
      Success
    else
      Failure "Append operator not working correctly"
  with
  | Meta.MetaParseError (_, msg) -> Failure ("Append operator failed: " ^ msg)
  | exn -> Failure ("Unexpected error: " ^ Printexc.to_string exn)

let test_comments_and_whitespace () =
  let content =
    {|
# This is a comment
version = "1.0"   # End of line comment

# Empty lines and spacing
description = "Test"

    # Indented comment
archive(byte) = "test.cma"
|}
  in
  try
    let pkg = parse_meta_string content "comments" in
    assert_equal ~expected:"1.0" ~actual:pkg.Meta.Pkg.version ~name:"version with comments"
  with
  | Meta.MetaParseError (_, msg) -> Failure ("Comments failed: " ^ msg)
  | exn -> Failure ("Unexpected error: " ^ Printexc.to_string exn)

let test_real_world_ppxlib () =
  (* Based on actual ppxlib META file *)
  let content =
    {|
version = "0.36.0"
description = ""
requires =
"compiler-libs.common
 ocaml-compiler-libs.shadow
 ppx_derivers"
archive(byte) = "ppxlib.cma"
archive(native) = "ppxlib.cmxa"
plugin(byte) = "ppxlib.cma"
plugin(native) = "ppxlib.cmxs"
|}
  in
  try
    let pkg = parse_meta_string content "ppxlib" in
    Success
  with
  | Meta.MetaParseError (_, msg) -> Failure ("Real ppxlib META failed: " ^ msg)
  | exn -> Failure ("Unexpected error: " ^ Printexc.to_string exn)

let test_malformed_syntax () =
  let content =
    {|
version = "1.0"
broken_field_no_value =
archive(byte) = 
requires = unix str
|}
  in
  (* This should fail to parse *)
  try
    let _ = parse_meta_string content "malformed" in
    Failure "Expected malformed syntax to fail, but it succeeded"
  with
  | Meta.MetaParseError _ -> Success
  | exn -> Failure ("Expected MetaParseError, got: " ^ Printexc.to_string exn)

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
    Failure "Libname parsing incorrect"

let test_ppx_stable_witness_findlib () =
  FindlibConf.load ();
  try
    let path, pkg = Meta.find_lib "ppx_stable_witness" in
    let stable_witness_libname = Libname.of_string "ppx_stable_witness.stable_witness" in
    let resolved_pkg = Meta.Pkg.find stable_witness_libname.Libname.subnames pkg in
    if resolved_pkg.Meta.Pkg.name = "stable_witness" then
      Success
    else
      Failure ("Expected stable_witness package, got: " ^ resolved_pkg.Meta.Pkg.name)
  with
  | Meta.LibraryNotFound name -> Failure ("LibraryNotFound: " ^ name)
  | exn -> Failure ("Unexpected error: " ^ Printexc.to_string exn)

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
      Failure ("Expected stable_witness from cache, got: " ^ cached_pkg.Meta.Pkg.name)
  with
  | Meta.LibraryNotFound name -> Failure ("LibraryNotFound in cache test: " ^ name)
  | Dependencies.DependencyMissing name -> Failure ("DependencyMissing in cache test: " ^ name)
  | exn -> Failure ("Cache test error: " ^ Printexc.to_string exn)

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
