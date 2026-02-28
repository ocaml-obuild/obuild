let err = ref 0

let assumeEq testname expected got =
  if expected = got then
    Printf.printf "SUCCESS %s\n" testname
  else (
    Printf.printf "FAILED %s Expected %s Got %s\n" testname expected got;
    err := !err + 1)

let assumeTrue testname v =
  if v then
    Printf.printf "SUCCESS %s\n" testname
  else (
    Printf.printf "FAILED %s Expected true Got false\n" testname;
    err := !err + 1)

let assumeRaises testname f =
  let raised = (try f (); false with _ -> true) in
  if raised then
    Printf.printf "SUCCESS %s\n" testname
  else (
    Printf.printf "FAILED %s Expected exception\n" testname;
    err := !err + 1)

let () =
  (* --- Hier.add_prefix tests (original) --- *)
  let b = Filepath.fp "src/b" in
  let b_abc = Hier.of_string "B.Abc" in
  let b_b_abc = Hier.add_prefix b b_abc in
  assumeEq "src/b + B.Abc" "src/b" (Filepath.fp_to_string b_b_abc);

  let b_abc = Filepath.fp "src/b/abc" in
  let b_abc_foo = Hier.of_string "B.Abc.Foo" in
  let b_abc_b_abc_foo = Hier.add_prefix b_abc b_abc_foo in
  assumeEq "src/b/abc + B.Abc.Foo" "src/b/abc" (Filepath.fp_to_string b_abc_b_abc_foo);

  (* --- Hier.of_string / to_string round-trip --- *)
  assumeEq "hier round-trip simple" "Foo" (Hier.to_string (Hier.of_string "Foo"));
  assumeEq "hier round-trip nested" "Foo.Bar.Baz"
    (Hier.to_string (Hier.of_string "Foo.Bar.Baz"));

  (* --- Hier.root / leaf / parent --- *)
  let hier_abc = Hier.of_string "A.B.C" in
  assumeEq "hier root" "A" (Modname.to_string (Hier.root hier_abc));
  assumeEq "hier leaf" "C" (Modname.to_string (Hier.leaf hier_abc));
  (match Hier.parent hier_abc with
  | Some p -> assumeEq "hier parent" "A.B" (Hier.to_string p)
  | None ->
      Printf.printf "FAILED hier parent: Expected Some, Got None\n";
      err := !err + 1);

  let hier_single = Hier.of_string "Solo" in
  (match Hier.parent hier_single with
  | None -> Printf.printf "SUCCESS hier parent of single\n"
  | Some p ->
      Printf.printf "FAILED hier parent of single: Expected None, Got %s\n"
        (Hier.to_string p);
      err := !err + 1);

  (* --- Hier.lvl --- *)
  assumeEq "hier lvl 0" "0" (string_of_int (Hier.lvl (Hier.of_string "X")));
  assumeEq "hier lvl 2" "2" (string_of_int (Hier.lvl (Hier.of_string "X.Y.Z")));

  (* --- Hier.to_dirpath --- *)
  assumeEq "hier to_dirpath single" "./" (Filepath.fp_to_string (Hier.to_dirpath (Hier.of_string "Foo")));
  assumeEq "hier to_dirpath nested" "foo/bar"
    (Filepath.fp_to_string (Hier.to_dirpath (Hier.of_string "Foo.Bar.Baz")));

  (* --- Hier.of_filename --- *)
  let hier_from_fn = Hier.of_filename (Filepath.fn "parser.ml") in
  assumeEq "hier of_filename" "Parser" (Hier.to_string hier_from_fn);

  (* --- Hier.add_prefix edge cases --- *)
  let empty_prefix = Filepath.fp "./" in
  let hier_deep = Hier.of_string "A.B.C" in
  let result = Hier.add_prefix empty_prefix hier_deep in
  assumeEq "add_prefix empty + A.B.C" "a/b" (Filepath.fp_to_string result);

  let prefix_no_overlap = Filepath.fp "lib/core" in
  let hier_xy = Hier.of_string "X.Y.Z" in
  let result2 = Hier.add_prefix prefix_no_overlap hier_xy in
  assumeEq "add_prefix no overlap" "lib/core/x/y" (Filepath.fp_to_string result2);

  (* --- Modname tests --- *)
  assumeEq "modname round-trip" "Foo" (Modname.to_string (Modname.of_string "Foo"));
  assumeEq "modname to_dir" "foo" (Modname.to_dir (Modname.of_string "Foo"));

  assumeRaises "modname empty" (fun () -> ignore (Modname.of_string ""));
  assumeRaises "modname lowercase" (fun () -> ignore (Modname.of_string "foo"));
  assumeRaises "modname invalid chars" (fun () -> ignore (Modname.of_string "Foo-bar"));

  assumeEq "modname to_filename" "foo.ml"
    (Filepath.fn_to_string (Modname.to_filename (Modname.of_string "Foo")));

  assumeEq "modname of_filename" "Parser"
    (Modname.to_string (Modname.of_filename (Filepath.fn "parser.ml")));

  (* --- Libname tests --- *)
  let lib_simple = Libname.of_string "unix" in
  assumeEq "libname simple" "unix" (Libname.to_string lib_simple);

  let lib_sub = Libname.of_string "base.shadow_stdlib" in
  assumeEq "libname sub" "base.shadow_stdlib" (Libname.to_string lib_sub);
  assumeEq "libname main_name" "base" lib_sub.Libname.main_name;
  assumeEq "libname subnames" "shadow_stdlib"
    (String.concat "." lib_sub.Libname.subnames);

  let lib_appended = Libname.append lib_simple "sub" in
  assumeEq "libname append" "unix.sub" (Libname.to_string lib_appended);

  (* Libname.of_string "" produces { main_name = ""; subnames = [] } rather than raising *)
  assumeEq "libname empty string" "" (Libname.to_string (Libname.of_string ""));

  (* --- Filepath tests --- *)
  assumeEq "fp absolute" "/usr/lib" (Filepath.fp_to_string (Filepath.fp "/usr/lib"));
  assumeEq "fp relative" "src/main" (Filepath.fp_to_string (Filepath.fp "src/main"));
  assumeEq "fp current" "./" (Filepath.fp_to_string (Filepath.fp "."));
  assumeEq "fp root" "/" (Filepath.fp_to_string (Filepath.fp "/"));

  assumeTrue "fp is_absolute" (Filepath.is_absolute (Filepath.fp "/usr"));
  assumeTrue "fp not absolute" (not (Filepath.is_absolute (Filepath.fp "src")));

  let combined = Filepath.(fp "src" </> fn "main.ml") in
  assumeEq "fp combine" "src/main.ml" (Filepath.fp_to_string combined);

  let concat = Filepath.(fp "a/b" <//> fp "c/d") in
  assumeEq "fp concat" "a/b/c/d" (Filepath.fp_to_string concat);

  assumeRaises "fn empty" (fun () -> ignore (Filepath.fn ""));
  assumeRaises "fn with slash" (fun () -> ignore (Filepath.fn "a/b"));

  if !err > 0 then
    exit 1
  else
    exit 0
