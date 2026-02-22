open Lib

let err = ref 0
let test_count = ref 0

let assumeEq testname expected got =
  incr test_count;
  if expected = got then
    Printf.printf "SUCCESS %s\n" testname
  else (
    Printf.printf "FAILED %s Expected %s Got %s\n" testname expected got;
    err := !err + 1)

let assumeTrue testname v =
  incr test_count;
  if v then
    Printf.printf "SUCCESS %s\n" testname
  else (
    Printf.printf "FAILED %s Expected true Got false\n" testname;
    err := !err + 1)

let assumeRaises testname f =
  incr test_count;
  let raised = (try f (); false with _ -> true) in
  if raised then
    Printf.printf "SUCCESS %s\n" testname
  else (
    Printf.printf "FAILED %s Expected exception\n" testname;
    err := !err + 1)

let () =
  (* Clean state before each run *)
  Generators.clear_custom_generators ();

  (* --- substitute_variables tests --- *)
  let src = Filepath.fp "src/parser.mly" in
  let dest = Filepath.fp "dist/build/lib/parser" in
  let result = Generators.substitute_variables
    ~src ~dest ~sources:[src] "${src}" in
  assumeEq "subst src" "src/parser.mly" result;

  let result = Generators.substitute_variables
    ~src ~dest ~sources:[src] "${dest}" in
  assumeEq "subst dest" "dist/build/lib/parser" result;

  let result = Generators.substitute_variables
    ~src ~dest ~sources:[src] "${base}" in
  assumeEq "subst base" "parser" result;

  let result = Generators.substitute_variables
    ~src ~dest ~sources:[src] "${srcdir}" in
  assumeEq "subst srcdir" "src" result;

  let result = Generators.substitute_variables
    ~src ~dest ~sources:[src] "${destdir}" in
  assumeEq "subst destdir" "dist/build/lib" result;

  let result = Generators.substitute_variables
    ~src ~dest ~sources:[src; Filepath.fp "src/tokens.mly"]
    "${sources}" in
  assumeEq "subst sources" "src/parser.mly src/tokens.mly" result;

  (* Multiple variables in one string *)
  let result = Generators.substitute_variables
    ~src ~dest ~sources:[src]
    "menhir --base ${dest} ${src}" in
  assumeEq "subst multiple" "menhir --base dist/build/lib/parser src/parser.mly" result;

  (* No variables *)
  let result = Generators.substitute_variables
    ~src ~dest ~sources:[src] "echo hello" in
  assumeEq "subst no vars" "echo hello" result;

  (* Repeated variable *)
  let result = Generators.substitute_variables
    ~src ~dest ~sources:[src] "${base}-${base}" in
  assumeEq "subst repeated" "parser-parser" result;

  (* --- substitute_output_pattern tests --- *)
  let result = Generators.substitute_output_pattern
    ~src:(Filepath.fp "parser.mly") "${base}.ml" in
  assumeEq "output pattern ml" "parser.ml" result;

  let result = Generators.substitute_output_pattern
    ~src:(Filepath.fp "parser.mly") "${base}.mli" in
  assumeEq "output pattern mli" "parser.mli" result;

  let result = Generators.substitute_output_pattern
    ~src:(Filepath.fp "lexer.mll") "${base}.ml" in
  assumeEq "output pattern lexer" "lexer.ml" result;

  (* No substitution *)
  let result = Generators.substitute_output_pattern
    ~src:(Filepath.fp "foo.atd") "version_info.ml" in
  assumeEq "output pattern literal" "version_info.ml" result;

  (* --- register_custom / get_all / find_generator_by_name tests --- *)
  Generators.clear_custom_generators ();

  let gen_menhir : Generators.custom = {
    custom_name = "menhir";
    custom_suffix = Some "mly";
    custom_command = "menhir --base ${dest} ${src}";
    custom_outputs = ["${base}.ml"; "${base}.mli"];
    custom_module_name = None;
  } in
  Generators.register_custom gen_menhir;

  let gen_lex : Generators.custom = {
    custom_name = "ocamllex";
    custom_suffix = Some "mll";
    custom_command = "ocamllex -o ${dest}.ml ${src}";
    custom_outputs = ["${base}.ml"];
    custom_module_name = None;
  } in
  Generators.register_custom gen_lex;

  let all = Generators.get_all () in
  assumeEq "get_all count" "2" (string_of_int (List.length all));

  (* --- is_generator_ext tests --- *)
  assumeTrue "is_generator_ext mly" (Generators.is_generator_ext "mly");
  assumeTrue "is_generator_ext mll" (Generators.is_generator_ext "mll");
  assumeTrue "is_generator_ext unknown" (not (Generators.is_generator_ext "xyz"));
  assumeTrue "is_generator_ext ml" (not (Generators.is_generator_ext "ml"));

  (* --- find_generator_by_name tests --- *)
  (match Generators.find_generator_by_name "menhir" with
  | Some g -> assumeEq "find_by_name menhir" "menhir" g.Generators.custom_name
  | None ->
      Printf.printf "FAILED find_by_name menhir: Expected Some, Got None\n";
      incr test_count; err := !err + 1);

  (match Generators.find_generator_by_name "nonexistent" with
  | None -> Printf.printf "SUCCESS find_by_name nonexistent\n"; incr test_count
  | Some _ ->
      Printf.printf "FAILED find_by_name nonexistent: Expected None, Got Some\n";
      incr test_count; err := !err + 1);

  (* --- Generator without suffix (generate-block-only) --- *)
  Generators.clear_custom_generators ();

  let gen_no_suffix : Generators.custom = {
    custom_name = "embed-version";
    custom_suffix = None;
    custom_command = "echo 'let version = \"1.0\"' > ${dest}.ml";
    custom_outputs = ["version_info.ml"];
    custom_module_name = Some "version_info";
  } in
  Generators.register_custom gen_no_suffix;

  (* Generator without suffix should not appear in get_all (auto-detection) *)
  let all_no_suffix = Generators.get_all () in
  assumeEq "no suffix not in get_all" "0" (string_of_int (List.length all_no_suffix));

  (* But should be findable by name *)
  (match Generators.find_generator_by_name "embed-version" with
  | Some _ -> Printf.printf "SUCCESS find no-suffix by name\n"; incr test_count
  | None ->
      Printf.printf "FAILED find no-suffix by name: Expected Some, Got None\n";
      incr test_count; err := !err + 1);

  (* --- get_all returns builtin type with correct suffix --- *)
  Generators.clear_custom_generators ();
  Generators.register_custom gen_menhir;

  let builtin = List.hd (Generators.get_all ()) in
  assumeEq "builtin suffix" "mly" builtin.Generators.suffix;

  (* modname identity (no custom_module_name) *)
  let m = Modname.of_string "Parser" in
  assumeEq "builtin modname identity" "Parser"
    (Modname.to_string (builtin.Generators.modname m));

  (* generated_files from outputs *)
  let gen_file = builtin.Generators.generated_files (Filepath.fn "parser.mly") "parser" in
  assumeEq "builtin generated_files" "parser.ml" (Filepath.fn_to_string gen_file);

  (* --- get_custom_outputs tests --- *)
  let outputs = Generators.get_custom_outputs gen_menhir
    ~src:(Filepath.fp "parser.mly") in
  assumeEq "custom_outputs count" "2" (string_of_int (List.length outputs));
  assumeEq "custom_outputs first" "parser.ml" (Filepath.fn_to_string (List.hd outputs));
  assumeEq "custom_outputs second" "parser.mli"
    (Filepath.fn_to_string (List.nth outputs 1));

  (* --- clear_custom_generators test --- *)
  Generators.register_custom gen_menhir;
  Generators.register_custom gen_lex;
  Generators.clear_custom_generators ();
  let all_after_clear = Generators.get_all () in
  assumeEq "clear generators" "0" (string_of_int (List.length all_after_clear));

  (* --- get_generator raises on unknown extension --- *)
  Generators.clear_custom_generators ();
  assumeRaises "get_generator not found"
    (fun () -> ignore (Generators.get_generator (Filepath.fp "foo.xyz")));

  (* --- register_customs (batch registration) --- *)
  Generators.clear_custom_generators ();
  Generators.register_customs [gen_menhir; gen_lex];
  let all_batch = Generators.get_all () in
  assumeEq "register_customs count" "2" (string_of_int (List.length all_batch));

  (* --- Summary --- *)
  Printf.printf "\n%d tests run, %d failures\n" !test_count !err;
  if !err > 0 then
    exit 1
  else
    exit 0
