open Test_build_helpers

(** Debug version of failing tests with verbose output *)

(** Test: ML change and incremental rebuild - with debug output *)
let test_ml_incremental_debug () =
  with_temp_build_project
    ~name:"ml_incremental_debug"
    ~files:[
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
      Printf.printf "Project directory: %s\n" dir;

      (* Initial build *)
      Printf.printf "\n=== Initial Configure ===\n";
      let (success, output) = run_obuild_command ~project_dir:dir ~command:"configure" ~args:[] in
      if not success then failwith ("Configure failed: " ^ output);
      Printf.printf "%s\n" output;

      Printf.printf "\n=== Initial Build ===\n";
      let (success, output) = run_obuild_command ~project_dir:dir ~command:"build" ~args:[] in
      if not success then failwith ("Initial build failed: " ^ output);
      Printf.printf "%s\n" output;

      (* Get initial mtimes *)
      let foo_cmo = dir ^ "/dist/build/lib-test/foo.cmo" in
      let bar_cmo = dir ^ "/dist/build/lib-test/bar.cmo" in
      let baz_cmo = dir ^ "/dist/build/lib-test/baz.cmo" in

      let foo_mtime1 = get_mtime foo_cmo in
      let bar_mtime1 = get_mtime bar_cmo in
      let baz_mtime1 = get_mtime baz_cmo in

      Printf.printf "\n=== Initial Mtimes ===\n";
      Printf.printf "foo.cmo: %s\n" (match foo_mtime1 with Some t -> string_of_float t | None -> "missing");
      Printf.printf "bar.cmo: %s\n" (match bar_mtime1 with Some t -> string_of_float t | None -> "missing");
      Printf.printf "baz.cmo: %s\n" (match baz_mtime1 with Some t -> string_of_float t | None -> "missing");

      Printf.printf "\nSleeping 1.1 seconds...\n";
      short_sleep ();

      (* Modify only foo.ml (no interface change) *)
      Printf.printf "\n=== Modifying foo.ml ===\n";
      write_file_with_dirs (dir ^ "/src/foo.ml") "let x = 99\n";
      Printf.printf "Changed foo.ml from 'let x = 42' to 'let x = 99'\n";

      (* Rebuild with verbose output *)
      Printf.printf "\n=== Rebuild ===\n";
      let (success, output) = run_obuild_command ~project_dir:dir ~command:"build" ~args:[] in
      if not success then failwith ("Rebuild failed: " ^ output);
      Printf.printf "%s\n" output;

      (* Get new mtimes *)
      let foo_mtime2 = get_mtime foo_cmo in
      let bar_mtime2 = get_mtime bar_cmo in
      let baz_mtime2 = get_mtime baz_cmo in

      Printf.printf "\n=== New Mtimes ===\n";
      Printf.printf "foo.cmo: %s\n" (match foo_mtime2 with Some t -> string_of_float t | None -> "missing");
      Printf.printf "bar.cmo: %s\n" (match bar_mtime2 with Some t -> string_of_float t | None -> "missing");
      Printf.printf "baz.cmo: %s\n" (match baz_mtime2 with Some t -> string_of_float t | None -> "missing");

      Printf.printf "\n=== Mtime Changes ===\n";
      (match (foo_mtime1, foo_mtime2) with
       | (Some t1, Some t2) -> Printf.printf "foo.cmo: %s (%.2f seconds)\n"
           (if t2 > t1 then "REBUILT" else "unchanged") (t2 -. t1)
       | _ -> Printf.printf "foo.cmo: error getting mtimes\n");

      (match (bar_mtime1, bar_mtime2) with
       | (Some t1, Some t2) -> Printf.printf "bar.cmo: %s (%.2f seconds)\n"
           (if t2 > t1 then "REBUILT" else "unchanged") (t2 -. t1)
       | _ -> Printf.printf "bar.cmo: error getting mtimes\n");

      (match (baz_mtime1, baz_mtime2) with
       | (Some t1, Some t2) -> Printf.printf "baz.cmo: %s (%.2f seconds)\n"
           (if t2 > t1 then "REBUILT" else "unchanged") (t2 -. t1)
       | _ -> Printf.printf "baz.cmo: error getting mtimes\n");

      Printf.printf "\n=== Analysis ===\n";
      Printf.printf "Expected: foo.cmo REBUILT, bar.cmo unchanged, baz.cmo unchanged\n";
      Printf.printf "Reason: foo.ml implementation changed, but interface did not change\n";
      Printf.printf "bar.ml depends on Foo's interface, not implementation\n";
    )

(** Test: C file rebuild - with debug output *)
let test_c_file_rebuild_debug () =
  with_temp_build_project
    ~name:"c_rebuild_debug"
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
      Printf.printf "Project directory: %s\n" dir;

      (* Initial build *)
      Printf.printf "\n=== Initial Build ===\n";
      let (success, output) = run_obuild_command ~project_dir:dir ~command:"configure" ~args:[] in
      if not success then failwith ("Configure failed: " ^ output);

      let (success, output) = run_obuild_command ~project_dir:dir ~command:"build" ~args:[] in
      if not success then failwith ("Initial build failed: " ^ output);
      Printf.printf "%s\n" output;

      (* Find C object and executable *)
      Printf.printf "\n=== Finding Build Artifacts ===\n";
      let dist_contents = Sys.readdir (dir ^ "/dist/build") in
      Printf.printf "dist/build contents: %s\n" (String.concat ", " (Array.to_list dist_contents));

      (* List all files in ctest directory *)
      let ctest_dir = dir ^ "/dist/build/ctest" in
      if Sys.file_exists ctest_dir && Sys.is_directory ctest_dir then (
        let ctest_contents = Sys.readdir ctest_dir in
        Printf.printf "dist/build/ctest contents: %s\n"
          (String.concat ", " (Array.to_list ctest_contents));
      ) else (
        Printf.printf "dist/build/ctest: NOT FOUND\n";
      );

      (* Try different possible locations for C object *)
      let possible_c_obj_paths = [
        dir ^ "/dist/build/ctest/cbits.c.o";
        dir ^ "/dist/build/exe-ctest/cbits.c.o";
        dir ^ "/dist/build/lib-test/cbits.c.o";
      ] in

      let c_obj = List.find_opt Sys.file_exists possible_c_obj_paths in
      let exe = dir ^ "/dist/build/ctest/ctest" in

      Printf.printf "\nC object file: %s\n"
        (match c_obj with Some p -> p ^ " (found)" | None -> "NOT FOUND");
      Printf.printf "Executable: %s\n" (if Sys.file_exists exe then exe ^ " (found)" else "NOT FOUND");

      (match c_obj with
       | None ->
           Printf.printf "\nERROR: Could not find C object file in any expected location\n";
           Printf.printf "Checked:\n";
           List.iter (fun p -> Printf.printf "  - %s\n" p) possible_c_obj_paths;
           failwith "C object file not found"
       | Some c_obj_path ->
           let obj_mtime1 = get_mtime c_obj_path in
           let exe_mtime1 = get_mtime exe in

           Printf.printf "\n=== Initial Mtimes ===\n";
           Printf.printf "C object: %s\n" (match obj_mtime1 with Some t -> string_of_float t | None -> "missing");
           Printf.printf "Executable: %s\n" (match exe_mtime1 with Some t -> string_of_float t | None -> "missing");

           Printf.printf "\nSleeping 1.1 seconds...\n";
           short_sleep ();

           (* Modify C file *)
           Printf.printf "\n=== Modifying C file ===\n";
           write_file_with_dirs (dir ^ "/src/cbits.c") "int add(int a, int b) { return a + b + 1; }\n";
           Printf.printf "Changed: return a + b; -> return a + b + 1;\n";

           (* Rebuild *)
           Printf.printf "\n=== Rebuild ===\n";
           let (success, output) = run_obuild_command ~project_dir:dir ~command:"build" ~args:[] in
           if not success then failwith ("Rebuild failed: " ^ output);
           Printf.printf "%s\n" output;

           (* Get new mtimes *)
           let obj_mtime2 = get_mtime c_obj_path in
           let exe_mtime2 = get_mtime exe in

           Printf.printf "\n=== New Mtimes ===\n";
           Printf.printf "C object: %s\n" (match obj_mtime2 with Some t -> string_of_float t | None -> "missing");
           Printf.printf "Executable: %s\n" (match exe_mtime2 with Some t -> string_of_float t | None -> "missing");

           Printf.printf "\n=== Mtime Changes ===\n";
           (match (obj_mtime1, obj_mtime2) with
            | (Some t1, Some t2) -> Printf.printf "C object: %s (%.2f seconds)\n"
                (if t2 > t1 then "REBUILT" else "unchanged") (t2 -. t1)
            | _ -> Printf.printf "C object: error getting mtimes\n");

           (match (exe_mtime1, exe_mtime2) with
            | (Some t1, Some t2) -> Printf.printf "Executable: %s (%.2f seconds)\n"
                (if t2 > t1 then "RELINKED" else "unchanged") (t2 -. t1)
            | _ -> Printf.printf "Executable: error getting mtimes\n");

           Printf.printf "\n=== Analysis ===\n";
           Printf.printf "Expected: C object REBUILT, Executable RELINKED\n";
      )
    )

let () =
  print_endline "";
  print_endline "Build Logic Debug Tests";
  print_endline "=======================";
  print_endline "";

  print_endline "=============================================================";
  print_endline "TEST 1: ML Incremental Rebuild";
  print_endline "=============================================================";
  test_ml_incremental_debug ();

  print_endline "";
  print_endline "=============================================================";
  print_endline "TEST 2: C File Rebuild";
  print_endline "=============================================================";
  test_c_file_rebuild_debug ();

  print_endline "";
  print_endline "Debug tests completed!";
