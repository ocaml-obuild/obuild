open Test_framework
open Test_build_helpers

(** Test 1: MLI change triggers CMI and ML rebuild *)
let test_mli_triggers_rebuild () =
  with_temp_build_project
    ~name:"mli_rebuild"
    ~files:[
      ("src/foo.mli", "val x : int\n");
      ("src/foo.ml", "let x = 42\n");
      ("src/bar.ml", "let y = Foo.x + 1\n");
    ]
    ~obuild_content:{|name: mli-test
version: 1.0
obuild-ver: 1

library foo
  modules: Foo, Bar
  src-dir: src
|}
    ~test_fn:(fun dir ->
      (* Initial build *)
      let (success, output) = run_obuild_command ~project_dir:dir ~command:"configure" ~args:[] in
      if not success then
        failwith ("Configure failed: " ^ output);

      let (success, output) = run_obuild_command ~project_dir:dir ~command:"build" ~args:[] in
      if not success then
        failwith ("Initial build failed: " ^ output);

      (* Get initial mtimes *)
      let foo_cmi = dir ^ "/dist/build/lib-foo/foo.cmi" in
      let foo_cmo = dir ^ "/dist/build/lib-foo/foo.cmo" in
      let bar_cmo = dir ^ "/dist/build/lib-foo/bar.cmo" in

      assert_file_exists foo_cmi;
      assert_file_exists foo_cmo;
      assert_file_exists bar_cmo;

      let cmi_mtime1 = get_mtime foo_cmi in
      let foo_mtime1 = get_mtime foo_cmo in
      let bar_mtime1 = get_mtime bar_cmo in

      (* Sleep to ensure different mtime *)
      short_sleep ();

      (* Modify .mli file content (add comment) *)
      write_file_with_dirs (dir ^ "/src/foo.mli") "(* changed *)\nval x : int\n";

      (* Rebuild *)
      let (success, output) = run_obuild_command ~project_dir:dir ~command:"build" ~args:[] in
      if not success then
        failwith ("Rebuild after mli change failed: " ^ output);

      (* Verify .cmi was rebuilt *)
      let cmi_mtime2 = get_mtime foo_cmi in
      assert_mtime_newer ~msg:"foo.cmi should be rebuilt when foo.mli changes" cmi_mtime1 cmi_mtime2;

      (* Verify .ml was recompiled (depends on own .cmi) *)
      let foo_mtime2 = get_mtime foo_cmo in
      assert_mtime_newer ~msg:"foo.ml should be recompiled when foo.mli changes" foo_mtime1 foo_mtime2;

      (* Verify dependent module was recompiled *)
      let bar_mtime2 = get_mtime bar_cmo in
      assert_mtime_newer ~msg:"bar.ml should be recompiled when dependency interface changes" bar_mtime1 bar_mtime2;
    )

(** Test 2: ML change only rebuilds that module (not dependencies) *)
(** Note: Tests bytecode mode - native code rebuilds are expected due to .cmx inlining *)
let test_ml_incremental_rebuild () =
  with_temp_build_project
    ~name:"ml_incremental"
    ~files:[
      ("src/foo.mli", "val x : int\n");  (* Explicit interface prevents .cmi regeneration *)
      ("src/foo.ml", "let x = 42\n");
      ("src/bar.ml", "let y = Foo.x + 1\n");
      ("src/baz.ml", "let z = Bar.y * 2\n");
    ]
    ~obuild_content:{|name: incremental-test
version: 1.0
obuild-ver: 1

library test
  modules: Foo, Bar, Baz
  src-dir: src
|}
    ~test_fn:(fun dir ->
      (* Configure for bytecode only to test incremental compilation *)
      let (success, _) = run_obuild_command ~project_dir:dir ~command:"configure"
        ~args:["--enable-library-bytecode"; "--disable-library-native"] in
      if not success then failwith "configure should succeed";

      let (success, _) = run_obuild_command ~project_dir:dir ~command:"build" ~args:[] in
      if not success then failwith "initial build should succeed";

      (* Get initial mtimes - check bytecode (.cmo) files *)
      let foo_cmo = dir ^ "/dist/build/lib-test/foo.cmo" in
      let bar_cmo = dir ^ "/dist/build/lib-test/bar.cmo" in
      let baz_cmo = dir ^ "/dist/build/lib-test/baz.cmo" in

      let foo_mtime1 = get_mtime foo_cmo in
      let bar_mtime1 = get_mtime bar_cmo in
      let baz_mtime1 = get_mtime baz_cmo in

      short_sleep ();

      (* Modify only foo.ml (no interface change) *)
      write_file_with_dirs (dir ^ "/src/foo.ml") "let x = 99\n";

      (* Rebuild *)
      let (success, _) = run_obuild_command ~project_dir:dir ~command:"build" ~args:[] in
      if not success then failwith "rebuild should succeed";

      (* Verify only foo.ml was rebuilt *)
      let foo_mtime2 = get_mtime foo_cmo in
      assert_mtime_newer ~msg:"foo.ml should be rebuilt" foo_mtime1 foo_mtime2;

      (* In bytecode mode, bar and baz should NOT rebuild (no interface change) *)
      let bar_mtime2 = get_mtime bar_cmo in
      let baz_mtime2 = get_mtime baz_cmo in

      assert_mtime_unchanged ~msg:"bar.ml should NOT rebuild in bytecode (foo interface unchanged)" bar_mtime1 bar_mtime2;
      assert_mtime_unchanged ~msg:"baz.ml should NOT rebuild in bytecode (foo interface unchanged)" baz_mtime1 baz_mtime2;
    )

(** Test 3: C file change triggers recompilation and relinking *)
let test_c_file_rebuild () =
  with_temp_build_project
    ~name:"c_rebuild"
    ~files:[
      ("src/cbits.c", "int add(int a, int b) { return a + b; }\n");
      ("src/cbits.h", "int add(int a, int b);\n");
      ("src/main.ml", "external add : int -> int -> int = \"add\"\nlet () = Printf.printf \"%d\\n\" (add 1 2)\n");
    ]
    ~obuild_content:{|name: c-test
version: 1.0
obuild-ver: 1

executable ctest
  main-is: main.ml
  src-dir: src
  c-sources: cbits.c
  c-dir: src
|}
    ~test_fn:(fun dir ->
      (* Initial build *)
      let (success, _) = run_obuild_command ~project_dir:dir ~command:"configure" ~args:[] in
      if not success then failwith "configure should succeed";

      let (success, _) = run_obuild_command ~project_dir:dir ~command:"build" ~args:[] in
      if not success then failwith "initial build should succeed";

      (* Get initial mtimes *)
      (* C objects are stored in the target's build directory *)
      let c_obj = dir ^ "/dist/build/ctest/cbits.c.o" in
      let exe = dir ^ "/dist/build/ctest/ctest" in

      assert_file_exists c_obj;
      assert_file_exists exe;

      let obj_mtime1 = get_mtime c_obj in
      let exe_mtime1 = get_mtime exe in

      short_sleep ();

      (* Modify C file *)
      write_file_with_dirs (dir ^ "/src/cbits.c") "int add(int a, int b) { return a + b + 1; }\n";

      (* Rebuild *)
      let (success, _) = run_obuild_command ~project_dir:dir ~command:"build" ~args:[] in
      if not success then failwith "rebuild should succeed";

      (* Verify .o was rebuilt *)
      let obj_mtime2 = get_mtime c_obj in
      assert_mtime_newer ~msg:"C object file should be rebuilt" obj_mtime1 obj_mtime2;

      (* Verify executable was relinked *)
      let exe_mtime2 = get_mtime exe in
      assert_mtime_newer ~msg:"Executable should be relinked" exe_mtime1 exe_mtime2;
    )

(** Test 4: Clean removes all build artifacts *)
let test_clean_build () =
  with_temp_build_project
    ~name:"clean_test"
    ~files:[
      ("src/foo.ml", "let x = 42\n");
    ]
    ~obuild_content:{|name: clean-test
version: 1.0
obuild-ver: 1

library foo
  modules: Foo
  src-dir: src
|}
    ~test_fn:(fun dir ->
      (* Build *)
      let (success, _) = run_obuild_command ~project_dir:dir ~command:"configure" ~args:[] in
      if not success then failwith "configure should succeed";

      let (success, _) = run_obuild_command ~project_dir:dir ~command:"build" ~args:[] in
      if not success then failwith "build should succeed";

      (* Verify artifacts exist *)
      let dist_dir = dir ^ "/dist" in
      assert_file_exists dist_dir;
      assert_file_exists (dir ^ "/dist/build/lib-foo/foo.cmi");

      (* Clean *)
      let (success, _) = run_obuild_command ~project_dir:dir ~command:"clean" ~args:[] in
      if not success then failwith "clean should succeed";

      (* Verify dist directory is cleaned *)
      (* Note: dist/ itself may still exist but should be empty or only contain setup *)
      let artifacts = [
        dir ^ "/dist/build/lib-foo/foo.cmi";
        dir ^ "/dist/build/lib-foo/foo.cmo";
        dir ^ "/dist/build/lib-foo/foo.cmx";
      ] in

      List.iter (fun artifact ->
        if Sys.file_exists artifact then
          failwith (Printf.sprintf "Artifact should be removed by clean: %s" artifact)
      ) artifacts;
    )

(** Test 5: Configure change triggers rebuild *)
let test_configure_rebuild () =
  with_temp_build_project
    ~name:"config_rebuild"
    ~files:[
      ("src/foo.ml", "let x = 42\n");
    ]
    ~obuild_content:{|name: config-test
version: 1.0
obuild-ver: 1

library foo
  modules: Foo
  src-dir: src
|}
    ~test_fn:(fun dir ->
      (* Initial build *)
      let (success, _) = run_obuild_command ~project_dir:dir ~command:"configure" ~args:[] in
      if not success then failwith "configure should succeed";

      let (success, _) = run_obuild_command ~project_dir:dir ~command:"build" ~args:[] in
      if not success then failwith "build should succeed";

      let foo_cmo = dir ^ "/dist/build/lib-foo/foo.cmo" in
      let mtime1 = get_mtime foo_cmo in

      short_sleep ();

      (* Reconfigure with different options *)
      let (success, _) = run_obuild_command ~project_dir:dir ~command:"configure"
        ~args:["--enable-library-debugging"] in
      if not success then failwith "reconfigure should succeed";

      (* Build again *)
      let (success, _) = run_obuild_command ~project_dir:dir ~command:"build" ~args:[] in
      if not success then failwith "rebuild should succeed";

      (* Verify module was rebuilt with new flags *)
      let mtime2 = get_mtime foo_cmo in
      assert_mtime_newer ~msg:"Module should rebuild after configure change" mtime1 mtime2;
    )

(** Test 6: Parallel build with dependencies *)
let test_parallel_build () =
  with_temp_build_project
    ~name:"parallel_build"
    ~files:[
      ("src/a.ml", "let a = 1\n");
      ("src/b.ml", "let b = 2\n");
      ("src/c.ml", "let c = A.a + B.b\n");
    ]
    ~obuild_content:{|name: parallel-test
version: 1.0
obuild-ver: 1

library test
  modules: A, B, C
  src-dir: src
|}
    ~test_fn:(fun dir ->
      (* Build with parallelism *)
      let (success, _) = run_obuild_command ~project_dir:dir ~command:"configure" ~args:[] in
      if not success then failwith "configure should succeed";

      let (success, output) = run_obuild_command ~project_dir:dir ~command:"build" ~args:["-j"; "2"] in
      if not success then
        failwith ("Parallel build failed: " ^ output);

      (* Verify all artifacts exist (build completed successfully) *)
      assert_file_exists (dir ^ "/dist/build/lib-test/a.cmo");
      assert_file_exists (dir ^ "/dist/build/lib-test/b.cmo");
      assert_file_exists (dir ^ "/dist/build/lib-test/c.cmo");

      (* If parallel build succeeded, dependencies were respected *)
      (* (C couldn't build before A and B) *)
    )

(** Run all build logic tests *)
let () =
  print_endline "";
  print_endline "Build Logic Tests";
  print_endline "=================";
  print_endline "";

  let tests = [
    ("mli_triggers_rebuild", test_mli_triggers_rebuild,
     "MLI change triggers CMI and dependent ML rebuilds");
    ("ml_incremental_rebuild", test_ml_incremental_rebuild,
     "ML change only rebuilds that module (incremental)");
    ("c_file_rebuild", test_c_file_rebuild,
     "C file change triggers recompilation and relinking");
    ("clean_build", test_clean_build,
     "Clean removes all build artifacts");
    ("configure_rebuild", test_configure_rebuild,
     "Configure change triggers rebuild");
    ("parallel_build", test_parallel_build,
     "Parallel build respects dependencies");
  ] in

  let run_test (name, test_fn, description) =
    Printf.printf "Running test: %s... " description;
    flush stdout;
    try
      test_fn ();
      print_endline "PASS";
      true
    with
    | Failure msg ->
        Printf.printf "FAIL\n  %s\n" msg;
        false
    | e ->
        Printf.printf "ERROR\n  %s\n" (Printexc.to_string e);
        false
  in

  let results = List.map run_test tests in
  let passed = List.filter (fun x -> x) results |> List.length in
  let total = List.length tests in

  print_endline "";
  Printf.printf "Results: %d/%d tests passed\n" passed total;

  if passed = total then
    exit 0
  else
    exit 1
