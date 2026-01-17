(** Comprehensive tests for the new obuild parser *)

open Obuild_lexer
open Obuild_parser

(* Import only specific types from Obuild_ast to avoid shadowing *)
module Ast = Obuild_ast

(** Test result tracking *)
let tests_run = ref 0
let tests_passed = ref 0
let tests_failed = ref 0

let test name f =
  incr tests_run;
  try
    f ();
    incr tests_passed;
    Printf.printf "  [PASS] %s\n" name
  with e ->
    incr tests_failed;
    Printf.printf "  [FAIL] %s: %s\n" name (Printexc.to_string e)

let assert_eq msg expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %s, got %s" msg
                (if expected = "" then "(empty)" else expected)
                (if actual = "" then "(empty)" else actual))

let assert_eq_int msg expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %d, got %d" msg expected actual)

let assert_eq_bool msg expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %b, got %b" msg expected actual)

let assert_true msg cond =
  if not cond then failwith msg

let assert_some msg = function
  | Some _ -> ()
  | None -> failwith (msg ^ ": expected Some, got None")

let assert_none msg = function
  | None -> ()
  | Some _ -> failwith (msg ^ ": expected None, got Some")

(* ============================================================ *)
(* LEXER TESTS *)
(* ============================================================ *)

let test_lexer () =
  Printf.printf "\n=== Lexer Tests ===\n";

  test "empty input" (fun () ->
    let tokens = tokenize "" in
    assert_eq_int "token count" 1 (List.length tokens);
    assert_true "should be EOF" ((List.hd tokens).tok = EOF)
  );

  test "blank lines only" (fun () ->
    let tokens = tokenize "\n\n  \n" in
    assert_eq_int "token count" 1 (List.length tokens);
    assert_true "should be EOF" ((List.hd tokens).tok = EOF)
  );

  test "comment lines" (fun () ->
    let tokens = tokenize "# this is a comment\n# another comment" in
    assert_eq_int "token count" 1 (List.length tokens)
  );

  test "simple key-value with colon" (fun () ->
    let tokens = tokenize "name: myproject" in
    assert_eq_int "token count" 2 (List.length tokens);
    match (List.hd tokens).tok with
    | KEY_VALUE (k, v) ->
      assert_eq "key" "name" k;
      assert_eq "value" "myproject" v
    | _ -> failwith "expected KEY_VALUE"
  );

  test "key-value with equals" (fun () ->
    let tokens = tokenize "version = 1.0.0" in
    match (List.hd tokens).tok with
    | KEY_VALUE (k, v) ->
      assert_eq "key" "version" k;
      assert_eq "value" "1.0.0" v
    | _ -> failwith "expected KEY_VALUE"
  );

  test "key-value with spaces" (fun () ->
    let tokens = tokenize "  description:   some text here  " in
    let t = List.hd tokens in
    assert_eq_int "indent" 2 t.indent;
    match t.tok with
    | KEY_VALUE (k, v) ->
      assert_eq "key" "description" k;
      assert_eq "value" "some text here" v
    | _ -> failwith "expected KEY_VALUE"
  );

  test "block header no args" (fun () ->
    let tokens = tokenize "library" in
    match (List.hd tokens).tok with
    | BLOCK (name, args) ->
      assert_eq "name" "library" name;
      assert_eq_int "args count" 0 (List.length args)
    | _ -> failwith "expected BLOCK"
  );

  test "block header with one arg" (fun () ->
    let tokens = tokenize "library mylib" in
    match (List.hd tokens).tok with
    | BLOCK (name, args) ->
      assert_eq "name" "library" name;
      assert_eq_int "args count" 1 (List.length args);
      assert_eq "arg" "mylib" (List.hd args)
    | _ -> failwith "expected BLOCK"
  );

  test "block header with multiple args" (fun () ->
    let tokens = tokenize "per file1.ml file2.ml" in
    match (List.hd tokens).tok with
    | BLOCK (name, args) ->
      assert_eq "name" "per" name;
      assert_eq_int "args count" 2 (List.length args)
    | _ -> failwith "expected BLOCK"
  );

  test "indentation tracking" (fun () ->
    let tokens = tokenize "library mylib\n  modules: A, B\n  src-dir: lib" in
    assert_eq_int "token count" 4 (List.length tokens);  (* 3 + EOF *)
    let t0 = List.nth tokens 0 in
    let t1 = List.nth tokens 1 in
    let t2 = List.nth tokens 2 in
    assert_eq_int "t0 indent" 0 t0.indent;
    assert_eq_int "t1 indent" 2 t1.indent;
    assert_eq_int "t2 indent" 2 t2.indent
  );

  test "mixed indentation" (fun () ->
    let input = "library mylib\n  modules: A\n    nested: value\n  back: here" in
    let tokens = tokenize input in
    let indents = List.map (fun t -> t.indent) (List.filter (fun t -> t.tok <> EOF) tokens) in
    assert_true "indents" (indents = [0; 2; 4; 2])
  );

  test "line numbers" (fun () ->
    let tokens = tokenize "line1: a\n\nline3: b\nline4: c" in
    let lines = List.map (fun t -> t.loc.line) (List.filter (fun t -> t.tok <> EOF) tokens) in
    assert_true "line numbers" (lines = [1; 3; 4])
  );

  ()

(* ============================================================ *)
(* PARSER BASIC TESTS *)
(* ============================================================ *)

let test_parser_basic () =
  Printf.printf "\n=== Parser Basic Tests ===\n";

  test "minimal project" (fun () ->
    let input = "name: test\nversion: 1.0\nobuild-ver: 1" in
    let proj = parse input in
    assert_eq "name" "test" proj.project_name.value;
    assert_eq "version" "1.0" proj.project_version.value;
    assert_eq_int "obuild-ver" 1 proj.project_obuild_ver.value
  );

  test "project metadata" (fun () ->
    let input = {|
name: myproject
version: 2.0.0
obuild-ver: 1
synopsis: A test project
description: This is a longer description
license: MIT
license-file: LICENSE
homepage: https://example.com
authors: Alice, Bob
|} in
    let proj = parse input in
    assert_eq "name" "myproject" proj.project_name.value;
    assert_eq "synopsis" "A test project" (Option.get proj.project_synopsis);
    assert_eq "license" "MIT" (Option.get proj.project_license);
    assert_eq "license-file" "LICENSE" (Option.get proj.project_license_file);
    assert_eq "homepage" "https://example.com" (Option.get proj.project_homepage);
    assert_eq_int "authors count" 2 (List.length proj.project_authors)
  );

  test "single author" (fun () ->
    let input = "name: x\nversion: 1\nobuild-ver: 1\nauthor: Alice" in
    let proj = parse input in
    assert_eq_int "authors count" 1 (List.length proj.project_authors);
    assert_eq "author" "Alice" (List.hd proj.project_authors)
  );

  test "extra-srcs and tools" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1
extra-srcs: file1.txt, file2.txt
tools: tool1, tool2
|} in
    let proj = parse input in
    assert_eq_int "extra-srcs" 2 (List.length proj.project_extra_srcs);
    assert_eq_int "tools" 2 (List.length proj.project_extra_tools)
  );

  test "configure-script" (fun () ->
    let input = "name: x\nversion: 1\nobuild-ver: 1\nconfigure-script: configure.sh" in
    let proj = parse input in
    assert_eq "configure-script" "configure.sh" (Option.get proj.project_configure_script)
  );

  test "ocaml-version" (fun () ->
    let input = "name: x\nversion: 1\nobuild-ver: 1\nocaml-version: >= 4.08" in
    let proj = parse input in
    assert_eq "ocaml-version" ">= 4.08" (Option.get proj.project_ocaml_ver)
  );

  ()

(* ============================================================ *)
(* LIBRARY TESTS *)
(* ============================================================ *)

let test_parser_library () =
  Printf.printf "\n=== Parser Library Tests ===\n";

  test "simple library" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

library mylib
  modules: A, B, C
  src-dir: lib
|} in
    let proj = parse input in
    assert_eq_int "libs count" 1 (List.length proj.project_libs);
    let lib = List.hd proj.project_libs in
    assert_eq "lib name" "mylib" lib.lib_name;
    assert_eq_int "modules count" 3 (List.length lib.lib_modules);
    assert_eq_int "src-dir count" 1 (List.length lib.lib_target.ocaml.src_dir)
  );

  test "library with build-deps" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

library mylib
  modules: A
  build-deps: unix, str, base (>= 0.14)
|} in
    let proj = parse input in
    let lib = List.hd proj.project_libs in
    let deps = lib.lib_target.ocaml.build_deps in
    assert_eq_int "deps count" 3 (List.length deps);
    let base_dep = List.nth deps 2 in
    assert_eq "dep name" "base" base_dep.dep_name;
    assert_eq "dep constraint" ">= 0.14" (Option.get base_dep.dep_constraint)
  );

  test "library with C settings" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

library mylib
  modules: A
  c-dir: csrc
  c-sources: foo.c, bar.c
  c-flags: -O2, -Wall
  c-libs: m, pthread
  c-pkgs: glib-2.0
|} in
    let proj = parse input in
    let lib = List.hd proj.project_libs in
    let c = lib.lib_target.c in
    assert_eq "c-dir" "csrc" (Option.get c.c_dir);
    assert_eq_int "c-sources" 2 (List.length c.c_sources);
    assert_eq_int "c-flags" 2 (List.length c.c_flags);
    assert_eq_int "c-libs" 2 (List.length c.c_libs);
    assert_eq_int "c-pkgs" 1 (List.length c.c_pkgs)
  );

  test "library with pack and syntax" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

library mylib
  modules: A
  pack: true
  syntax: true
|} in
    let proj = parse input in
    let lib = List.hd proj.project_libs in
    assert_eq_bool "pack" true lib.lib_pack;
    assert_eq_bool "syntax" true lib.lib_syntax
  );

  test "library with stdlib none" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

library mylib
  modules: A
  stdlib: none
|} in
    let proj = parse input in
    let lib = List.hd proj.project_libs in
    assert_true "stdlib is none" (lib.lib_target.ocaml.stdlib = Some Ast.Stdlib_None)
  );

  test "library with oflags" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

library mylib
  modules: A
  oflags: -w, +a-4
|} in
    let proj = parse input in
    let lib = List.hd proj.project_libs in
    assert_eq_int "oflags" 2 (List.length lib.lib_target.ocaml.oflags)
  );

  test "library buildable/installable" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

library mylib
  modules: A
  buildable: false
  installable: $flag_install
|} in
    let proj = parse input in
    let lib = List.hd proj.project_libs in
    assert_true "buildable" (lib.lib_target.buildable = Ast.Bool_const false);
    assert_true "installable" (lib.lib_target.installable = Ast.Bool_var "flag_install")
  );

  test "multiple libraries" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

library lib1
  modules: A

library lib2
  modules: B
|} in
    let proj = parse input in
    assert_eq_int "libs count" 2 (List.length proj.project_libs)
  );

  ()

(* ============================================================ *)
(* CSTUBS TESTS *)
(* ============================================================ *)

let test_parser_cstubs () =
  Printf.printf "\n=== Parser Cstubs Tests ===\n";

  test "library with cstubs" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

library mylib
  modules: Bindings, C, Types_generated
  build-deps: ctypes, ctypes.stubs

  cstubs
    external-library-name: mylib_stubs
    type-description: Bindings.Types -> Types_gen
    function-description: Bindings.Functions -> Funcs_gen
    generated-types: Types_generated
    generated-entry-point: C
    headers: string.h, mylib.h
|} in
    let proj = parse input in
    let lib = List.hd proj.project_libs in
    assert_some "cstubs" lib.lib_cstubs;
    let cstubs = Option.get lib.lib_cstubs in
    assert_eq "external-library-name" "mylib_stubs" cstubs.cstubs_external_lib_name;
    assert_some "type-description" cstubs.cstubs_type_desc;
    let type_desc = Option.get cstubs.cstubs_type_desc in
    assert_eq "type functor" "Bindings.Types" type_desc.cstubs_functor;
    assert_eq "type instance" "Types_gen" type_desc.cstubs_instance;
    assert_some "function-description" cstubs.cstubs_func_desc;
    assert_eq "generated-types" "Types_generated" cstubs.cstubs_generated_types;
    assert_eq "generated-entry-point" "C" cstubs.cstubs_generated_entry;
    assert_eq_int "headers" 2 (List.length cstubs.cstubs_headers)
  );

  test "cstubs minimal" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

library mylib
  modules: A

  cstubs
    external-library-name: foo
|} in
    let proj = parse input in
    let lib = List.hd proj.project_libs in
    assert_some "cstubs" lib.lib_cstubs;
    let cstubs = Option.get lib.lib_cstubs in
    assert_eq "external-library-name" "foo" cstubs.cstubs_external_lib_name;
    assert_none "type-description" cstubs.cstubs_type_desc;
    assert_none "function-description" cstubs.cstubs_func_desc
  );

  ()

(* ============================================================ *)
(* EXECUTABLE TESTS *)
(* ============================================================ *)

let test_parser_executable () =
  Printf.printf "\n=== Parser Executable Tests ===\n";

  test "simple executable" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

executable myexe
  main-is: main.ml
  src-dir: src
|} in
    let proj = parse input in
    assert_eq_int "exes count" 1 (List.length proj.project_exes);
    let exe = List.hd proj.project_exes in
    assert_eq "exe name" "myexe" exe.exe_name;
    assert_eq "main-is" "main.ml" exe.exe_main;
    assert_eq_int "src-dir" 1 (List.length exe.exe_target.ocaml.src_dir)
  );

  test "executable with deps" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

executable myexe
  main-is: main.ml
  build-deps: unix, cmdliner
|} in
    let proj = parse input in
    let exe = List.hd proj.project_exes in
    assert_eq_int "deps" 2 (List.length exe.exe_target.ocaml.build_deps)
  );

  test "multiple executables" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

executable exe1
  main-is: main1.ml

executable exe2
  main-is: main2.ml
|} in
    let proj = parse input in
    assert_eq_int "exes count" 2 (List.length proj.project_exes)
  );

  ()

(* ============================================================ *)
(* TEST TARGET TESTS *)
(* ============================================================ *)

let test_parser_test () =
  Printf.printf "\n=== Parser Test Target Tests ===\n";

  test "simple test" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

test mytest
  main-is: test_main.ml
  src-dir: tests
|} in
    let proj = parse input in
    assert_eq_int "tests count" 1 (List.length proj.project_tests);
    let t = List.hd proj.project_tests in
    assert_eq "test name" "mytest" t.test_name;
    assert_eq "main-is" "test_main.ml" t.test_main
  );

  test "test with rundir" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

test mytest
  main-is: test.ml
  run-dir: test_data
|} in
    let proj = parse input in
    let t = List.hd proj.project_tests in
    assert_eq "rundir" "test_data" (Option.get t.test_rundir)
  );

  ()

(* ============================================================ *)
(* PER BLOCK TESTS *)
(* ============================================================ *)

let test_parser_per () =
  Printf.printf "\n=== Parser Per Block Tests ===\n";

  test "library with per block" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

library mylib
  modules: A, B

  per A
    build-deps: special_lib
    oflags: -w, -40
|} in
    let proj = parse input in
    let lib = List.hd proj.project_libs in
    assert_eq_int "per blocks" 1 (List.length lib.lib_target.per);
    let per = List.hd lib.lib_target.per in
    assert_eq_int "per files" 1 (List.length per.per_files);
    assert_eq "per file" "A" (List.hd per.per_files);
    assert_eq_int "per deps" 1 (List.length per.per_build_deps);
    assert_eq_int "per oflags" 2 (List.length per.per_oflags)
  );

  test "multiple per blocks" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

library mylib
  modules: A, B, C

  per A
    oflags: -w, -40

  per B C
    pp: ppx_deriving
|} in
    let proj = parse input in
    let lib = List.hd proj.project_libs in
    assert_eq_int "per blocks" 2 (List.length lib.lib_target.per);
    let per2 = List.nth lib.lib_target.per 1 in
    assert_eq_int "per2 files" 2 (List.length per2.per_files);
    assert_eq "per2 pp" "ppx_deriving" (Option.get per2.per_pp)
  );

  ()

(* ============================================================ *)
(* SUB-LIBRARY TESTS *)
(* ============================================================ *)

let test_parser_sublib () =
  Printf.printf "\n=== Parser Sub-library Tests ===\n";

  test "library with sublibrary" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

library mylib
  modules: A

  sublib internal
    modules: B, C
    src-dir: internal
|} in
    let proj = parse input in
    let lib = List.hd proj.project_libs in
    assert_eq_int "subs" 1 (List.length lib.lib_subs);
    let sub = List.hd lib.lib_subs in
    assert_eq "sub name" "internal" sub.lib_name;
    assert_eq_int "sub modules" 2 (List.length sub.lib_modules)
  );

  ()

(* ============================================================ *)
(* FLAG TESTS *)
(* ============================================================ *)

let test_parser_flag () =
  Printf.printf "\n=== Parser Flag Tests ===\n";

  test "flag definition" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

flag debug
  description: Enable debug mode
  default: false
|} in
    let proj = parse input in
    assert_eq_int "flags" 1 (List.length proj.project_flags);
    let flag = List.hd proj.project_flags in
    assert_eq "flag name" "debug" flag.flag_name;
    assert_eq "flag desc" "Enable debug mode" flag.flag_description;
    assert_eq_bool "flag default" false flag.flag_default
  );

  ()

(* ============================================================ *)
(* REAL FILE TESTS *)
(* ============================================================ *)

let test_real_files () =
  Printf.printf "\n=== Real File Tests ===\n";

  test "ctypes_test.obuild" (fun () ->
    let input = {|
name: ctypes_test
version: 0.1.0
obuild-ver: 1

library mylib
  modules: Bindings, C, Types_generated
  build-deps: ctypes, ctypes.stubs
  src-dir: lib

  cstubs
    external-library-name: mylib_stubs
    type-description: Bindings.Types -> Types_gen
    function-description: Bindings.Functions -> Funcs_gen
    generated-types: Types_generated
    generated-entry-point: C
    headers: string.h

executable test_mylib
  main-is: main.ml
  src-dir: bin
  build-deps: mylib, integers, ctypes
|} in
    let proj = parse input in
    assert_eq "name" "ctypes_test" proj.project_name.value;
    assert_eq "version" "0.1.0" proj.project_version.value;
    assert_eq_int "obuild-ver" 1 proj.project_obuild_ver.value;
    assert_eq_int "libs" 1 (List.length proj.project_libs);
    assert_eq_int "exes" 1 (List.length proj.project_exes);

    let lib = List.hd proj.project_libs in
    assert_eq "lib name" "mylib" lib.lib_name;
    assert_eq_int "lib modules" 3 (List.length lib.lib_modules);
    assert_some "lib cstubs" lib.lib_cstubs;

    let exe = List.hd proj.project_exes in
    assert_eq "exe name" "test_mylib" exe.exe_name;
    assert_eq "exe main" "main.ml" exe.exe_main;
    assert_eq_int "exe deps" 3 (List.length exe.exe_target.ocaml.build_deps)
  );

  test "complex project" (fun () ->
    let input = {|
name: myproject
version: 1.0.0
obuild-ver: 1
synopsis: A complex project
license: BSD-3-Clause
authors: Alice, Bob, Charlie
homepage: https://github.com/example/myproject

# Main library
library core
  modules: Types, Utils, Engine
  build-deps: base (>= 0.14), stdio, unix
  src-dir: lib/core
  oflags: -w, +a-4-40-42

  per Engine
    build-deps: threads
    pp: ppx_deriving.show

# CLI executable
executable mycli
  main-is: main.ml
  src-dir: bin
  build-deps: core, cmdliner (>= 1.0)

# Tests
test unit_tests
  main-is: test_unit.ml
  src-dir: tests
  build-deps: core, alcotest

test integration_tests
  main-is: test_integration.ml
  src-dir: tests
  build-deps: core, alcotest
  run-dir: test_fixtures

# Feature flag
flag debug
  description: Build with debug info
  default: false
|} in
    let proj = parse input in
    assert_eq "name" "myproject" proj.project_name.value;
    assert_eq_int "authors" 3 (List.length proj.project_authors);
    assert_eq_int "libs" 1 (List.length proj.project_libs);
    assert_eq_int "exes" 1 (List.length proj.project_exes);
    assert_eq_int "tests" 2 (List.length proj.project_tests);
    assert_eq_int "flags" 1 (List.length proj.project_flags);

    let lib = List.hd proj.project_libs in
    assert_eq_int "lib modules" 3 (List.length lib.lib_modules);
    assert_eq_int "lib deps" 3 (List.length lib.lib_target.ocaml.build_deps);
    assert_eq_int "lib per" 1 (List.length lib.lib_target.per);

    let exe = List.hd proj.project_exes in
    assert_eq_int "exe deps" 2 (List.length exe.exe_target.ocaml.build_deps);

    let test1 = List.hd proj.project_tests in
    assert_none "test1 rundir" test1.test_rundir;

    let test2 = List.nth proj.project_tests 1 in
    assert_eq "test2 rundir" "test_fixtures" (Option.get test2.test_rundir)
  );

  ()

(* ============================================================ *)
(* EDGE CASES *)
(* ============================================================ *)

let test_edge_cases () =
  Printf.printf "\n=== Edge Case Tests ===\n";

  test "empty value" (fun () ->
    let input = "name: x\nversion: 1\nobuild-ver: 1\ndescription:" in
    let proj = parse input in
    assert_eq "description" "" (Option.get proj.project_description)
  );

  test "value with colons" (fun () ->
    let input = "name: x\nversion: 1\nobuild-ver: 1\nhomepage: https://example.com:8080/path" in
    let proj = parse input in
    assert_eq "homepage" "https://example.com:8080/path" (Option.get proj.project_homepage)
  );

  test "dependency with complex constraint" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

library mylib
  modules: A
  build-deps: foo (>= 1.0 && < 2.0)
|} in
    let proj = parse input in
    let lib = List.hd proj.project_libs in
    let dep = List.hd lib.lib_target.ocaml.build_deps in
    assert_eq "constraint" ">= 1.0 && < 2.0" (Option.get dep.dep_constraint)
  );

  test "tabs as indentation" (fun () ->
    let input = "name: x\nversion: 1\nobuild-ver: 1\nlibrary mylib\n\tmodules: A" in
    let proj = parse input in
    assert_eq_int "libs" 1 (List.length proj.project_libs)
  );

  test "mixed content" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1

# comment before library
library mylib
  modules: A  # inline comments not supported, this is part of value

  # comment in library
  src-dir: lib
|} in
    let proj = parse input in
    let lib = List.hd proj.project_libs in
    (* Note: inline comment becomes part of value - lexer doesn't handle inline comments *)
    assert_eq_int "src-dir" 1 (List.length lib.lib_target.ocaml.src_dir)
  );

  test "unknown fields ignored" (fun () ->
    let input = {|
name: x
version: 1
obuild-ver: 1
unknown-field: some value

library mylib
  modules: A
  unknown-lib-field: ignored
|} in
    let proj = parse input in
    assert_eq "name" "x" proj.project_name.value;
    assert_eq_int "libs" 1 (List.length proj.project_libs)
  );

  ()

(* ============================================================ *)
(* MAIN *)
(* ============================================================ *)

let () =
  Printf.printf "Running new parser tests...\n";

  test_lexer ();
  test_parser_basic ();
  test_parser_library ();
  test_parser_cstubs ();
  test_parser_executable ();
  test_parser_test ();
  test_parser_per ();
  test_parser_sublib ();
  test_parser_flag ();
  test_real_files ();
  test_edge_cases ();

  Printf.printf "\n=== Summary ===\n";
  Printf.printf "Tests run: %d\n" !tests_run;
  Printf.printf "Passed: %d\n" !tests_passed;
  Printf.printf "Failed: %d\n" !tests_failed;

  if !tests_failed > 0 then
    exit 1
  else
    Printf.printf "\nAll tests passed!\n"
