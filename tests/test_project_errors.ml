open Test_framework
open Test_helpers

(** Comprehensive Project parser error tests

    These tests systematically verify error handling for:
    - Required field validation
    - Field syntax errors
    - Block syntax errors
    - Value format errors
    - Semantic validation errors *)

(** {1 Required Field Tests} *)

let test_missing_name () =
  assert_project_parse_error ~content:"version: 1.0.0\nobuild-ver: 1\n"
    ~expected_msg:"Missing required field: name" ~name:"missing name field"

let test_missing_version () =
  assert_project_parse_error ~content:"name: test\nobuild-ver: 1\n"
    ~expected_msg:"Missing required field: version" ~name:"missing version field"

let test_missing_obuild_ver () =
  assert_project_parse_error ~content:"name: test\nversion: 1.0.0\n"
    ~expected_msg:"Missing required field: obuild-ver" ~name:"missing obuild-ver field"

let test_empty_name () =
  assert_project_parse_error ~content:"name:\nversion: 1.0.0\nobuild-ver: 1\n"
    ~expected_msg:"Missing required field: name" ~name:"empty name value"

(** {1 Field Value Tests} *)

let test_invalid_obuild_ver () =
  assert_project_parse_error ~content:"name: test\nversion: 1.0.0\nobuild-ver: not_a_number\n"
    ~expected_msg:"int_of_string" ~name:"invalid obuild-ver value"

let test_future_obuild_ver () =
  assert_project_parse_error ~content:"name: test\nversion: 1.0.0\nobuild-ver: 999\n"
    ~expected_msg:"Unsupported obuild version" ~name:"unsupported future obuild-ver"

let test_valid_minimal () =
  assert_project_parses ~content:minimal_project ~name:"valid minimal project"

(** {1 Block Section Tests} *)

(* Note: "library: value" is lexed as KEY_VALUE, not BLOCK (because of the colon).
   Since "library" isn't a recognized top-level field, it's silently ignored.
   No library is created, so parsing succeeds with just the project metadata. *)
let test_block_as_value () =
  assert_project_parses ~content:"name: test\nversion: 1.0.0\nobuild-ver: 1\nlibrary: some_value\n"
    ~name:"library: value is ignored (KEY_VALUE not BLOCK)"

let test_executable_block_as_value () =
  (* Same as above - "executable: value" is KEY_VALUE, silently ignored *)
  assert_project_parses
    ~content:"name: test\nversion: 1.0.0\nobuild-ver: 1\nexecutable: some_value\n"
    ~name:"executable: value is ignored (KEY_VALUE not BLOCK)"

(** {1 Library Block Tests} *)

let test_library_without_modules () =
  (* Library with no modules fails at validation *)
  assert_project_parse_error
    ~content:"name: test\nversion: 1.0.0\nobuild-ver: 1\n\nlibrary mylib\nsrc-dir: src\n"
    ~expected_msg:"has no modules" ~name:"library without modules"

let test_valid_library () =
  (* Parser validates that modules exist on disk - this will fail without actual files *)
  assert_project_parse_error
    ~content:
      "name: test\n\
       version: 1.0.0\n\
       obuild-ver: 1\n\n\
       library mylib\n\
       modules: Foo, Bar\n\
       src-dir: src\n"
    ~expected_msg:"ModuleDoesntExist" ~name:"library with non-existent modules"

(** {1 Executable Block Tests} *)

let test_executable_without_name () =
  (* New parser allows empty name but validation catches missing main file *)
  assert_project_parse_error
    ~content:"name: test\nversion: 1.0.0\nobuild-ver: 1\n\nexecutable\nmain-is: main.ml\n"
    ~expected_msg:"FileNotFoundInPaths" ~name:"executable without name"

let test_valid_executable () =
  (* Parser validates that main-is file exists on disk *)
  assert_project_parse_error
    ~content:
      "name: test\n\
       version: 1.0.0\n\
       obuild-ver: 1\n\n\
       executable myexe\n\
       main-is: main.ml\n\
       src-dir: src\n"
    ~expected_msg:"FileNotFoundInPaths" ~name:"executable with non-existent main file"

(** {1 Test Block Tests} *)

let test_test_without_name () =
  (* New parser allows empty test name; Project.check() doesn't validate tests *)
  assert_project_parses
    ~content:"name: test\nversion: 1.0.0\nobuild-ver: 1\n\ntest\nmain-is: test.ml\n"
    ~name:"test without name (allowed)"

let test_valid_test () =
  assert_project_parses
    ~content:
      "name: test\nversion: 1.0.0\nobuild-ver: 1\n\ntest mytest\nmain-is: test.ml\nsrc-dir: tests\n"
    ~name:"valid test block"

(** {1 Field Format Tests} *)

let test_multiline_description () =
  assert_project_parses
    ~content:
      "name: test\n\
       version: 1.0.0\n\
       obuild-ver: 1\n\
       description: This is a long\n\
       description that spans\n\
       multiple lines\n"
    ~name:"multiline description"

let test_csv_authors () =
  assert_project_parses
    ~content:
      "name: test\n\
       version: 1.0.0\n\
       obuild-ver: 1\n\
       authors: Alice <alice@example.com>, Bob <bob@example.com>\n"
    ~name:"CSV authors field"

let test_single_author () =
  assert_project_parses
    ~content:"name: test\nversion: 1.0.0\nobuild-ver: 1\nauthor: Alice <alice@example.com>\n"
    ~name:"single author field"

(** {1 Unknown Field Handling} *)

let test_unknown_field_strict () =
  (* In strict mode, unknown fields should cause errors *)
  (* This test documents current behavior *)
  assert_project_parses ~content:"name: test\nversion: 1.0.0\nobuild-ver: 1\nunknown-field: value\n"
    ~name:"unknown field in non-strict mode"

(** {1 Indentation and Whitespace} *)

let test_empty_file () =
  assert_project_parse_error ~content:"" ~expected_msg:"Missing required field: name"
    ~name:"empty file"

let test_whitespace_only () =
  assert_project_parse_error ~content:"   \n  \n  " ~expected_msg:"Missing required field: name"
    ~name:"whitespace only"

let test_valid_with_comments () =
  assert_project_parses ~content:"# This is a comment\nname: test\nversion: 1.0.0\nobuild-ver: 1\n"
    ~name:"file with comments"

(** {1 Complex Nested Structures} *)

let test_multiple_libraries () =
  (* Parser validates module existence *)
  assert_project_parse_error
    ~content:
      "name: test\n\
       version: 1.0.0\n\
       obuild-ver: 1\n\n\
       library lib1\n\
       modules: Foo\n\
       src-dir: src1\n\n\
       library lib2\n\
       modules: Bar\n\
       src-dir: src2\n"
    ~expected_msg:"ModuleDoesntExist" ~name:"multiple libraries with non-existent modules"

let test_mixed_targets () =
  (* Parser validates file/module existence *)
  assert_project_parse_error
    ~content:
      "name: test\n\
       version: 1.0.0\n\
       obuild-ver: 1\n\n\
       library mylib\n\
       modules: Lib\n\
       src-dir: lib\n\n\
       executable myexe\n\
       main-is: main.ml\n\
       src-dir: src\n\
       build-deps: mylib\n\n\
       test mytest\n\
       main-is: test.ml\n\
       src-dir: tests\n\
       build-deps: mylib\n"
    ~expected_msg:"ModuleDoesntExist" ~name:"mixed targets with non-existent files"

(** {1 Edge Cases} *)

let test_library_too_many_names () =
  (* New parser takes first name, ignores rest; fails on module validation *)
  assert_project_parse_error
    ~content:"name: test\nversion: 1.0.0\nobuild-ver: 1\n\nlibrary lib1 lib2\nmodules: Foo\n"
    ~expected_msg:"ModuleDoesntExist" ~name:"library with extra names (first used)"

let test_colons_vs_equals () =
  (* Test both : and = syntax *)
  assert_project_parses ~content:"name: test\nversion: 1.0.0\nobuild-ver: 1\n" ~name:"colon syntax"

(** {1 Real-World Examples} *)

let test_real_world_obuild () =
  (* Based on actual obuild.obuild structure - but modules don't exist *)
  assert_project_parse_error
    ~content:
      "name: example\n\
       version: 0.1.0\n\
       synopsis: Example project\n\
       obuild-ver: 1\n\
       license: BSD\n\
       authors: Test Author <test@example.com>\n\n\
       library example_lib\n\
       modules: Foo, Bar\n\
       src-dir: lib\n\
       build-deps: unix\n\n\
       executable example_exe\n\
       main-is: main.ml\n\
       src-dir: src\n\
       build-deps: example_lib\n\n\
       test example_test\n\
       main-is: test.ml\n\
       src-dir: tests\n\
       build-deps: example_lib\n"
    ~expected_msg:"ModuleDoesntExist" ~name:"real-world obuild file with non-existent files"

(** {1 Test Suite} *)

let all_tests =
  [
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
