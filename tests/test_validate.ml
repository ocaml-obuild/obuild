(** Tests for the validation/transformation module *)

open Obuild_validate

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
    failwith (Printf.sprintf "%s: expected '%s', got '%s'" msg expected actual)

let assert_eq_int msg expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %d, got %d" msg expected actual)

let assert_true msg cond = if not cond then failwith msg

let assert_raises msg f =
  try
    f ();
    failwith (msg ^ ": expected exception, but none was raised")
  with
  | Validation_error _ -> () (* expected *)
  | Failure _ -> () (* also acceptable *)
  | e -> failwith (msg ^ ": unexpected exception: " ^ Printexc.to_string e)

(* Helper for Option in older OCaml *)
let option_is_some = function Some _ -> true | None -> false
let option_get = function Some x -> x | None -> failwith "option_get: None"

(* ============================================================ *)
(* BASIC CONVERSION TESTS *)
(* ============================================================ *)

let test_basic_conversion () =
  Printf.printf "\n=== Basic Conversion Tests ===\n";

  test "minimal project" (fun () ->
      let input = "\
name: test\n\
version: 1.0\n\
obuild-ver: 1\n" in
      let proj = parse_and_convert input in
      assert_eq "name" "test" proj.Project.name;
      assert_eq "version" "1.0" proj.Project.version;
      assert_eq_int "obuild-ver" 1 proj.Project.obuild_ver);

  test "project with metadata" (fun () ->
      let input = "\
name: myproject\n\
version: 2.0.0\n\
obuild-ver: 1\n\
synopsis: A test project\n\
description: Longer description\n\
license: MIT\n\
homepage: https://example.com\n\
authors: Alice, Bob\n"
      in
      let proj = parse_and_convert input in
      assert_eq "synopsis" "A test project" proj.Project.synopsis;
      assert_eq "description" "Longer description" proj.Project.description;
      assert_eq "license" "MIT" proj.Project.license;
      assert_eq "homepage" "https://example.com" proj.Project.homepage;
      assert_eq_int "authors" 2 (List.length proj.Project.authors));

  ()

(* ============================================================ *)
(* LIBRARY CONVERSION TESTS *)
(* ============================================================ *)

let test_library_conversion () =
  Printf.printf "\n=== Library Conversion Tests ===\n";

  test "simple library" (fun () ->
      let input = "\
name: x\n\
version: 1\n\
obuild-ver: 1\n\
\n\
library mylib\n\
  modules: A, B, C\n\
  src-dir: lib\n"
      in
      let proj = parse_and_convert input in
      assert_eq_int "libs count" 1 (List.length proj.Project.libs);
      let lib = List.hd proj.Project.libs in
      assert_eq "lib name" "mylib" (Libname.to_string lib.Project.Library.name);
      assert_eq_int "modules" 3 (List.length lib.Project.Library.modules);
      assert_eq_int "src-dir" 1
        (List.length lib.Project.Library.target.Target.target_obits.Target.target_srcdir));

  test "library with build-deps" (fun () ->
      let input = "\
name: x\n\
version: 1\n\
obuild-ver: 1\n\
\n\
library mylib\n\
  modules: A\n\
  build-deps: unix, str\n"
      in
      let proj = parse_and_convert input in
      let lib = List.hd proj.Project.libs in
      let deps = lib.Project.Library.target.Target.target_obits.Target.target_builddeps in
      assert_eq_int "deps count" 2 (List.length deps));

  test "library with C settings" (fun () ->
      let input = "\
name: x\n\
version: 1\n\
obuild-ver: 1\n\
\n\
library mylib\n\
  modules: A\n\
  c-dir: csrc\n\
  c-sources: foo.c\n\
  c-flags: -O2\n"
      in
      let proj = parse_and_convert input in
      let lib = List.hd proj.Project.libs in
      let cbits = lib.Project.Library.target.Target.target_cbits in
      assert_eq_int "c-sources" 1 (List.length cbits.Target.target_csources);
      assert_eq_int "c-flags" 1 (List.length cbits.Target.target_cflags));

  test "library with pack" (fun () ->
      let input = "\
name: x\n\
version: 1\n\
obuild-ver: 1\n\
\n\
library mylib\n\
  modules: A\n\
  pack: true\n" in
      let proj = parse_and_convert input in
      let lib = List.hd proj.Project.libs in
      assert_true "pack" lib.Project.Library.pack);

  ()

(* ============================================================ *)
(* EXECUTABLE CONVERSION TESTS *)
(* ============================================================ *)

let test_executable_conversion () =
  Printf.printf "\n=== Executable Conversion Tests ===\n";

  test "simple executable" (fun () ->
      let input = "\
name: x\n\
version: 1\n\
obuild-ver: 1\n\
\n\
executable myexe\n\
  main-is: main.ml\n\
  src-dir: src\n"
      in
      let proj = parse_and_convert input in
      assert_eq_int "exes count" 1 (List.length proj.Project.exes);
      let exe = List.hd proj.Project.exes in
      assert_eq "name" "myexe" exe.Project.Executable.name;
      assert_eq "main" "main.ml" (Filepath.fn_to_string exe.Project.Executable.main));

  test "executable with deps" (fun () ->
      let input = "\
name: x\n\
version: 1\n\
obuild-ver: 1\n\
\n\
executable myexe\n\
  main-is: main.ml\n\
  build-deps: unix, cmdliner\n"
      in
      let proj = parse_and_convert input in
      let exe = List.hd proj.Project.exes in
      let deps = exe.Project.Executable.target.Target.target_obits.Target.target_builddeps in
      assert_eq_int "deps" 2 (List.length deps));

  ()

(* ============================================================ *)
(* TEST TARGET CONVERSION TESTS *)
(* ============================================================ *)

let test_test_conversion () =
  Printf.printf "\n=== Test Target Conversion Tests ===\n";

  test "simple test" (fun () ->
      let input = "\
name: x\n\
version: 1\n\
obuild-ver: 1\n\
\n\
test mytest\n\
  main-is: test_main.ml\n" in
      let proj = parse_and_convert input in
      assert_eq_int "tests count" 1 (List.length proj.Project.tests);
      let t = List.hd proj.Project.tests in
      assert_eq "name" "mytest" t.Project.Test.name;
      assert_eq "main" "test_main.ml" (Filepath.fn_to_string t.Project.Test.main));

  test "test with rundir" (fun () ->
      let input = "\
name: x\n\
version: 1\n\
obuild-ver: 1\n\
\n\
test mytest\n\
  main-is: test.ml\n\
  run-dir: fixtures\n"
      in
      let proj = parse_and_convert input in
      let t = List.hd proj.Project.tests in
      assert_true "rundir is some" (option_is_some t.Project.Test.rundir));

  ()

(* ============================================================ *)
(* CSTUBS CONVERSION TESTS *)
(* ============================================================ *)

let test_cstubs_conversion () =
  Printf.printf "\n=== Cstubs Conversion Tests ===\n";

  test "library with cstubs" (fun () ->
      let input = "\
name: x\n\
version: 1\n\
obuild-ver: 1\n\
\n\
library mylib\n\
  modules: Bindings, C\n\
  build-deps: ctypes\n\
\n\
  cstubs\n\
    external-library-name: mylib_stubs\n\
    type-description: Bindings.Types -> Types_gen\n\
    function-description: Bindings.Functions -> Funcs_gen\n\
    headers: string.h\n"
      in
      let proj = parse_and_convert input in
      let lib = List.hd proj.Project.libs in
      assert_true "cstubs present" (option_is_some lib.Project.Library.target.Target.target_cstubs);
      let cstubs = option_get lib.Project.Library.target.Target.target_cstubs in
      assert_eq "external-library-name" "mylib_stubs" cstubs.Target.cstubs_external_library_name;
      assert_true "type-description" (option_is_some cstubs.Target.cstubs_type_description);
      assert_true "function-description" (option_is_some cstubs.Target.cstubs_function_description);
      assert_eq_int "headers" 1 (List.length cstubs.Target.cstubs_headers));

  test "cstubs auto-adds generated module" (fun () ->
      let input = "\
name: x\n\
version: 1\n\
obuild-ver: 1\n\
\n\
library mylib\n\
  modules: Bindings\n\
\n\
  cstubs\n\
    external-library-name: foo\n"
      in
      let proj = parse_and_convert input in
      let lib = List.hd proj.Project.libs in
      (* Should have Bindings + auto-generated Foo_generated + C + Types_generated *)
      assert_eq_int "modules" 4 (List.length lib.Project.Library.modules));
  ()

(* ============================================================ *)
(* FLAG CONVERSION TESTS *)
(* ============================================================ *)

let test_flag_conversion () =
  Printf.printf "\n=== Flag Conversion Tests ===\n";

  test "flag definition" (fun () ->
      let input = "\
name: x\n\
version: 1\n\
obuild-ver: 1\n\
\n\
flag debug\n\
  description: Enable debug mode\n\
  default: true\n"
      in
      let proj = parse_and_convert input in
      assert_eq_int "flags" 1 (List.length proj.Project.flags);
      let flag = List.hd proj.Project.flags in
      assert_eq "flag name" "debug" flag.Project.Flag.name;
      assert_eq "flag desc" "Enable debug mode" flag.Project.Flag.description);

  ()

(* ============================================================ *)
(* VALIDATION ERROR TESTS *)
(* ============================================================ *)

let test_validation_errors () =
  Printf.printf "\n=== Validation Error Tests ===\n";

  test "missing name" (fun () ->
      let input = "\
version: 1.0\n\
obuild-ver: 1\n" in
      assert_raises "missing name" (fun () -> ignore (parse_and_convert input)));

  test "missing version" (fun () ->
      let input = "\
name: x\n\
obuild-ver: 1\n" in
      assert_raises "missing version" (fun () -> ignore (parse_and_convert input)));

  test "missing obuild-ver" (fun () ->
      let input = "\
name: x\n\
version: 1.0\n" in
      assert_raises "missing obuild-ver" (fun () -> ignore (parse_and_convert input)));

  test "library with no modules" (fun () ->
      let input = "\
name: x\n\
version: 1\n\
obuild-ver: 1\n\
\n\
library mylib\n\
  src-dir: lib\n" in
      assert_raises "no modules" (fun () -> ignore (parse_and_convert input)));

  test "executable with no main" (fun () ->
      let input = "\
name: x\n\
version: 1\n\
obuild-ver: 1\n\
\n\
executable myexe\n\
  src-dir: src\n" in
      assert_raises "no main" (fun () -> ignore (parse_and_convert input)));

  ()

(* ============================================================ *)
(* COMPLEX PROJECT TESTS *)
(* ============================================================ *)

let test_complex_project () =
  Printf.printf "\n=== Complex Project Tests ===\n";

  test "full project" (fun () ->
      let input = "\
name: myproject\n\
version: 1.0.0\n\
obuild-ver: 1\n\
synopsis: A complex project\n\
license: BSD-3-Clause\n\
authors: Alice, Bob\n\
homepage: https://github.com/example/myproject\n\
\n\
library core\n\
  modules: Types, Utils, Engine\n\
  build-deps: base, stdio, unix\n\
  src-dir: lib/core\n\
\n\
  per Engine\n\
    build-deps: threads\n\
\n\
executable mycli\n\
  main-is: main.ml\n\
  src-dir: bin\n\
  build-deps: core, cmdliner\n\
\n\
test unit_tests\n\
  main-is: test_unit.ml\n\
  src-dir: tests\n\
  build-deps: core, alcotest\n\
\n\
flag debug\n\
  description: Build with debug info\n\
  default: false\n"
      in
      let proj = parse_and_convert input in
      assert_eq "name" "myproject" proj.Project.name;
      assert_eq_int "libs" 1 (List.length proj.Project.libs);
      assert_eq_int "exes" 1 (List.length proj.Project.exes);
      assert_eq_int "tests" 1 (List.length proj.Project.tests);
      assert_eq_int "flags" 1 (List.length proj.Project.flags);

      let lib = List.hd proj.Project.libs in
      assert_eq_int "lib modules" 3 (List.length lib.Project.Library.modules);
      assert_eq_int "lib extras" 1 (List.length lib.Project.Library.target.Target.target_extras));

  ()

(* ============================================================ *)
(* MAIN *)
(* ============================================================ *)

let () =
  Printf.printf "Running validation tests...\n";

  test_basic_conversion ();
  test_library_conversion ();
  test_executable_conversion ();
  test_test_conversion ();
  test_cstubs_conversion ();
  test_flag_conversion ();
  test_validation_errors ();
  test_complex_project ();

  Printf.printf "\n=== Summary ===\n";
  Printf.printf "Tests run: %d\n" !tests_run;
  Printf.printf "Passed: %d\n" !tests_passed;
  Printf.printf "Failed: %d\n" !tests_failed;

  if !tests_failed > 0 then
    exit 1
  else
    Printf.printf "\nAll tests passed!\n"
