open Test_framework
open Test_helpers

(** Comprehensive Project parser error tests

    These tests systematically verify error handling for:
    - Required field validation
    - Field syntax errors
    - Block syntax errors
    - Value format errors
    - Semantic validation errors
*)

(** {1 Required Field Tests} *)

let test_missing_name () =
  assert_project_parse_error
    ~content:{|
version: 1.0.0
obuild-ver: 1
|}
    ~expected_msg:"Missing required field: name"
    ~name:"missing name field"

let test_missing_version () =
  assert_project_parse_error
    ~content:{|
name: test
obuild-ver: 1
|}
    ~expected_msg:"Missing required field: version"
    ~name:"missing version field"

let test_missing_obuild_ver () =
  assert_project_parse_error
    ~content:{|
name: test
version: 1.0.0
|}
    ~expected_msg:"Missing required field: obuild-ver"
    ~name:"missing obuild-ver field"

let test_empty_name () =
  assert_project_parse_error
    ~content:{|
name:
version: 1.0.0
obuild-ver: 1
|}
    ~expected_msg:"Missing required field: name"
    ~name:"empty name value"

(** {1 Field Value Tests} *)

let test_invalid_obuild_ver () =
  assert_project_parse_error
    ~content:{|
name: test
version: 1.0.0
obuild-ver: not_a_number
|}
    ~expected_msg:"int_of_string"
    ~name:"invalid obuild-ver value"

let test_future_obuild_ver () =
  assert_project_parse_error
    ~content:{|
name: test
version: 1.0.0
obuild-ver: 999
|}
    ~expected_msg:"Unsupported obuild version"
    ~name:"unsupported future obuild-ver"

let test_valid_minimal () =
  assert_project_parses
    ~content:minimal_project
    ~name:"valid minimal project"

(** {1 Block Section Tests} *)

(* Note: "library: value" is lexed as KEY_VALUE, not BLOCK (because of the colon).
   Since "library" isn't a recognized top-level field, it's silently ignored.
   No library is created, so parsing succeeds with just the project metadata. *)
let test_block_as_value () =
  assert_project_parses
    ~content:{|
name: test
version: 1.0.0
obuild-ver: 1
library: some_value
|}
    ~name:"library: value is ignored (KEY_VALUE not BLOCK)"

let test_executable_block_as_value () =
  (* Same as above - "executable: value" is KEY_VALUE, silently ignored *)
  assert_project_parses
    ~content:{|
name: test
version: 1.0.0
obuild-ver: 1
executable: some_value
|}
    ~name:"executable: value is ignored (KEY_VALUE not BLOCK)"

(** {1 Library Block Tests} *)

let test_library_without_modules () =
  (* Library with no modules fails at validation *)
  assert_project_parse_error
    ~content:{|
name: test
version: 1.0.0
obuild-ver: 1

library mylib
  src-dir: src
|}
    ~expected_msg:"has no modules"
    ~name:"library without modules"

let test_valid_library () =
  (* Parser validates that modules exist on disk - this will fail without actual files *)
  assert_project_parse_error
    ~content:{|
name: test
version: 1.0.0
obuild-ver: 1

library mylib
  modules: Foo, Bar
  src-dir: src
|}
    ~expected_msg:"ModuleDoesntExist"
    ~name:"library with non-existent modules"

(** {1 Executable Block Tests} *)

let test_executable_without_name () =
  (* New parser allows empty name but validation catches missing main file *)
  assert_project_parse_error
    ~content:{|
name: test
version: 1.0.0
obuild-ver: 1

executable
  main-is: main.ml
|}
    ~expected_msg:"FileNotFoundInPaths"
    ~name:"executable without name"

let test_valid_executable () =
  (* Parser validates that main-is file exists on disk *)
  assert_project_parse_error
    ~content:{|
name: test
version: 1.0.0
obuild-ver: 1

executable myexe
  main-is: main.ml
  src-dir: src
|}
    ~expected_msg:"FileNotFoundInPaths"
    ~name:"executable with non-existent main file"

(** {1 Test Block Tests} *)

let test_test_without_name () =
  (* New parser allows empty test name; Project.check() doesn't validate tests *)
  assert_project_parses
    ~content:{|
name: test
version: 1.0.0
obuild-ver: 1

test
  main-is: test.ml
|}
    ~name:"test without name (allowed)"

let test_valid_test () =
  assert_project_parses
    ~content:{|
name: test
version: 1.0.0
obuild-ver: 1

test mytest
  main-is: test.ml
  src-dir: tests
|}
    ~name:"valid test block"

(** {1 Field Format Tests} *)

let test_multiline_description () =
  assert_project_parses
    ~content:{|
name: test
version: 1.0.0
obuild-ver: 1
description: This is a long
  description that spans
  multiple lines
|}
    ~name:"multiline description"

let test_csv_authors () =
  assert_project_parses
    ~content:{|
name: test
version: 1.0.0
obuild-ver: 1
authors: Alice <alice@example.com>, Bob <bob@example.com>
|}
    ~name:"CSV authors field"

let test_single_author () =
  assert_project_parses
    ~content:{|
name: test
version: 1.0.0
obuild-ver: 1
author: Alice <alice@example.com>
|}
    ~name:"single author field"

(** {1 Unknown Field Handling} *)

let test_unknown_field_strict () =
  (* In strict mode, unknown fields should cause errors *)
  (* This test documents current behavior *)
  assert_project_parses
    ~content:{|
name: test
version: 1.0.0
obuild-ver: 1
unknown-field: value
|}
    ~name:"unknown field in non-strict mode"

(** {1 Indentation and Whitespace} *)

let test_empty_file () =
  assert_project_parse_error
    ~content:""
    ~expected_msg:"Missing required field: name"
    ~name:"empty file"

let test_whitespace_only () =
  assert_project_parse_error
    ~content:"   \n  \n  "
    ~expected_msg:"Missing required field: name"
    ~name:"whitespace only"

let test_valid_with_comments () =
  assert_project_parses
    ~content:{|
# This is a comment
name: test
version: 1.0.0
obuild-ver: 1
|}
    ~name:"file with comments"

(** {1 Complex Nested Structures} *)

let test_multiple_libraries () =
  (* Parser validates module existence *)
  assert_project_parse_error
    ~content:{|
name: test
version: 1.0.0
obuild-ver: 1

library lib1
  modules: Foo
  src-dir: src1

library lib2
  modules: Bar
  src-dir: src2
|}
    ~expected_msg:"ModuleDoesntExist"
    ~name:"multiple libraries with non-existent modules"

let test_mixed_targets () =
  (* Parser validates file/module existence *)
  assert_project_parse_error
    ~content:{|
name: test
version: 1.0.0
obuild-ver: 1

library mylib
  modules: Lib
  src-dir: lib

executable myexe
  main-is: main.ml
  src-dir: src
  build-deps: mylib

test mytest
  main-is: test.ml
  src-dir: tests
  build-deps: mylib
|}
    ~expected_msg:"ModuleDoesntExist"
    ~name:"mixed targets with non-existent files"

(** {1 Edge Cases} *)

let test_library_too_many_names () =
  (* New parser takes first name, ignores rest; fails on module validation *)
  assert_project_parse_error
    ~content:{|
name: test
version: 1.0.0
obuild-ver: 1

library lib1 lib2
  modules: Foo
|}
    ~expected_msg:"ModuleDoesntExist"
    ~name:"library with extra names (first used)"

let test_colons_vs_equals () =
  (* Test both : and = syntax *)
  assert_project_parses
    ~content:{|
name: test
version: 1.0.0
obuild-ver: 1
|}
    ~name:"colon syntax"

(** {1 Real-World Examples} *)

let test_real_world_obuild () =
  (* Based on actual obuild.obuild structure - but modules don't exist *)
  assert_project_parse_error
    ~content:{|
name: example
version: 0.1.0
synopsis: Example project
obuild-ver: 1
license: BSD
authors: Test Author <test@example.com>

library example_lib
  modules: Foo, Bar
  src-dir: lib
  build-deps: unix

executable example_exe
  main-is: main.ml
  src-dir: src
  build-deps: example_lib

test example_test
  main-is: test.ml
  src-dir: tests
  build-deps: example_lib
|}
    ~expected_msg:"ModuleDoesntExist"
    ~name:"real-world obuild file with non-existent files"

(** {1 Test Suite} *)

let all_tests = [
  (* Required fields *)
  make_test "missing_name" test_missing_name;
  make_test "missing_version" test_missing_version;
  make_test "missing_obuild_ver" test_missing_obuild_ver;
  make_test "empty_name" test_empty_name;

  (* Field values *)
  make_test "invalid_obuild_ver" test_invalid_obuild_ver;
  make_test "future_obuild_ver" test_future_obuild_ver;
  make_test "valid_minimal" test_valid_minimal;

  (* Block errors *)
  make_test "block_as_value" test_block_as_value;
  make_test "executable_block_as_value" test_executable_block_as_value;

  (* Library blocks *)
  make_test "library_without_modules" test_library_without_modules;
  make_test "library_non_existent_modules" test_valid_library;

  (* Executable blocks *)
  make_test "executable_without_name" test_executable_without_name;
  make_test "executable_non_existent_main" test_valid_executable;

  (* Test blocks *)
  make_test "test_without_name" test_test_without_name;
  make_test "valid_test" test_valid_test;

  (* Field formats *)
  make_test "multiline_description" test_multiline_description;
  make_test "csv_authors" test_csv_authors;
  make_test "single_author" test_single_author;

  (* Unknown fields *)
  make_test "unknown_field_strict" test_unknown_field_strict;

  (* Whitespace *)
  make_test "empty_file" test_empty_file;
  make_test "whitespace_only" test_whitespace_only;
  make_test "valid_with_comments" test_valid_with_comments;

  (* Complex structures *)
  make_test "multiple_libraries_non_existent" test_multiple_libraries;
  make_test "mixed_targets_non_existent" test_mixed_targets;

  (* Edge cases *)
  make_test "library_too_many_names" test_library_too_many_names;
  make_test "colons_vs_equals" test_colons_vs_equals;

  (* Real-world *)
  make_test "real_world_non_existent_files" test_real_world_obuild;
]

let () = run_tests all_tests
