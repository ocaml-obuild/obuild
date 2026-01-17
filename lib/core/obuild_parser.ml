(** Parser for .obuild files

    Consumes tokens from the lexer and builds the AST.
    No validation - just structural parsing.
*)

open Obuild_lexer
open Obuild_ast

(** Parser error *)
exception Parser_error of loc * string

(** Parser state *)
type state = {
  tokens: located_token list;
  mutable pos: int;
}

(** Create parser state from tokens *)
let make_state tokens = { tokens; pos = 0 }

(** Get current token *)
let current st =
  if st.pos < List.length st.tokens then
    List.nth st.tokens st.pos
  else
    let loc = { line = 0; col = 0 } in
    { tok = EOF; loc; indent = 0 }

(** Advance to next token *)
let advance st = st.pos <- st.pos + 1

(** Peek at current token without consuming *)
let peek st = current st

(** Check if at end *)
let at_end st = (current st).tok = EOF

(** Raise parser error at current location *)
let error st msg =
  let t = current st in
  raise (Parser_error (t.loc, msg))

(** Expect a specific indentation level, return tokens at that level *)
let rec collect_block st base_indent =
  let t = peek st in
  if at_end st || t.indent <= base_indent then
    []
  else (
    advance st;
    t :: collect_block st base_indent
  )

(** Parse a comma-separated list *)
let parse_list s =
  let s = String.trim s in
  if s = "" then []
  else
    (* Split on commas first, then trim each *)
    let parts = String.split_on_char ',' s in
    let parts = List.map String.trim parts in
    (* Filter empty strings *)
    List.filter (fun s -> s <> "") parts

(** Parse a whitespace-separated list (for flags/args) *)
let parse_words s =
  let s = String.trim s in
  if s = "" then []
  else
    (* Split on spaces *)
    let parts = String.split_on_char ' ' s in
    (* Filter empty strings *)
    List.filter (fun s -> s <> "") parts

(** Parse a dependency: "name" or "name (>= 1.0)" *)
let parse_dependency s =
  let s = String.trim s in
  (* Look for opening paren *)
  match String.index_opt s '(' with
  | None -> { dep_name = s; dep_constraint = None }
  | Some i ->
    let name = String.trim (String.sub s 0 i) in
    let rest = String.sub s (i + 1) (String.length s - i - 1) in
    (* Find closing paren *)
    let constraint_str = match String.index_opt rest ')' with
      | None -> String.trim rest
      | Some j -> String.trim (String.sub rest 0 j)
    in
    { dep_name = name; dep_constraint = Some constraint_str }

(** Parse a list of dependencies *)
let parse_dependencies s =
  List.map parse_dependency (parse_list s)

(** Parse extra-dep: "A -> B" or "A before B" or "A then B" *)
let parse_extra_dep s =
  let s = String.trim s in
  (* Try different separators *)
  let try_split sep =
    match String.split_on_char sep.[0] s with
    | [a; b] when String.length sep = 1 ->
      Some { before = String.trim a; after = String.trim b }
    | _ -> None
  in
  (* Try " -> " first by looking for it *)
  let arrow_pos =
    let rec find i =
      if i + 4 > String.length s then None
      else if String.sub s i 4 = " -> " then Some i
      else find (i + 1)
    in
    find 0
  in
  match arrow_pos with
  | Some i ->
    let a = String.trim (String.sub s 0 i) in
    let b = String.trim (String.sub s (i + 4) (String.length s - i - 4)) in
    { before = a; after = b }
  | None ->
    (* Try "before" or "then" *)
    let words = String.split_on_char ' ' s in
    let words = List.filter (fun w -> w <> "") words in
    match words with
    | [a; "before"; b] | [a; "then"; b] -> { before = a; after = b }
    | [a; b] -> { before = a; after = b }
    | _ -> { before = s; after = "" }  (* fallback, validation will catch it *)

(** Parse stdlib value *)
let parse_stdlib s =
  match String.lowercase_ascii (String.trim s) with
  | "none" | "no" -> Some Stdlib_None
  | "standard" -> Some Stdlib_Standard
  | "core" -> Some Stdlib_Core
  | _ -> None

(** Parse runtime bool *)
let parse_runtime_bool s =
  match String.lowercase_ascii (String.trim s) with
  | "true" -> Bool_const true
  | "false" -> Bool_const false
  | s when String.length s > 0 && s.[0] = '$' ->
    Bool_var (String.sub s 1 (String.length s - 1))
  | s -> Bool_var s

(** Parse cstubs description: "Functor.Path -> Instance" *)
let parse_cstubs_desc s =
  let s = String.trim s in
  let arrow = " -> " in
  let rec find_arrow i =
    if i + 4 > String.length s then None
    else if String.sub s i 4 = arrow then Some i
    else find_arrow (i + 1)
  in
  match find_arrow 0 with
  | Some i ->
    let functor_path = String.trim (String.sub s 0 i) in
    let instance = String.trim (String.sub s (i + 4) (String.length s - i - 4)) in
    Some { cstubs_functor = functor_path; cstubs_instance = instance }
  | None -> None

(** Parse C settings from key-value pairs *)
let parse_c_setting c key value =
  match String.lowercase_ascii key with
  | "cdir" | "c-dir" ->
    { c with c_dir = Some value }
  | "csources" | "c-sources" ->
    { c with c_sources = c.c_sources @ parse_list value }
  | "cflags" | "c-flags" | "ccopts" | "ccopt" | "c-opts" ->
    { c with c_flags = c.c_flags @ parse_list value }
  | "c-libs" ->
    { c with c_libs = c.c_libs @ parse_list value }
  | "c-libpaths" ->
    { c with c_lib_paths = c.c_lib_paths @ parse_list value }
  | "c-pkgs" ->
    { c with c_pkgs = c.c_pkgs @ parse_dependencies value }
  | _ -> c

(** Parse OCaml settings from key-value pairs *)
let parse_ocaml_setting o key value =
  match String.lowercase_ascii key with
  | "path" | "srcdir" | "src-dir" ->
    { o with src_dir = o.src_dir @ parse_list value }
  | "builddepends" | "builddeps" | "build-deps" ->
    { o with build_deps = o.build_deps @ parse_dependencies value }
  | "preprocessor" | "pp" ->
    { o with pp = Some value }
  | "extra-deps" ->
    { o with extra_deps = o.extra_deps @ List.map parse_extra_dep (parse_list value) }
  | "oflags" ->
    { o with oflags = o.oflags @ parse_list value }
  | "stdlib" ->
    { o with stdlib = parse_stdlib value }
  | _ -> o

(** Parse common target setting *)
let parse_target_setting target key value =
  match String.lowercase_ascii key with
  | "buildable" ->
    { target with buildable = parse_runtime_bool value }
  | "installable" ->
    { target with installable = parse_runtime_bool value }
  | _ ->
    let ocaml' = parse_ocaml_setting target.ocaml key value in
    let c' = parse_c_setting target.c key value in
    { target with ocaml = ocaml'; c = c' }

(** Parse cstubs block *)
let parse_cstubs_block tokens =
  let rec loop cstubs = function
    | [] -> cstubs
    | t :: rest ->
      match t.tok with
      | KEY_VALUE (key, value) ->
        let cstubs' = match String.lowercase_ascii key with
          | "external-library-name" ->
            { cstubs with cstubs_external_lib_name = value }
          | "type-description" ->
            { cstubs with cstubs_type_desc = parse_cstubs_desc value }
          | "function-description" ->
            { cstubs with cstubs_func_desc = parse_cstubs_desc value }
          | "generated-types" ->
            { cstubs with cstubs_generated_types = value }
          | "generated-entry-point" ->
            { cstubs with cstubs_generated_entry = value }
          | "headers" ->
            { cstubs with cstubs_headers = cstubs.cstubs_headers @ parse_list value }
          | "concurrency" ->
            let concurrency = match String.lowercase_ascii value with
              | "sequential" -> Obuild_ast.Cstubs_sequential
              | "unlocked" -> Obuild_ast.Cstubs_unlocked
              | "lwt_jobs" | "lwt-jobs" -> Obuild_ast.Cstubs_lwt_jobs
              | "lwt_preemptive" | "lwt-preemptive" -> Obuild_ast.Cstubs_lwt_preemptive
              | _ -> failwith (Printf.sprintf "Unknown concurrency policy: %s (expected: sequential, unlocked, lwt_jobs, lwt_preemptive)" value)
            in
            { cstubs with cstubs_concurrency = concurrency }
          | "errno" ->
            let errno = match String.lowercase_ascii value with
              | "ignore" | "ignore_errno" | "ignore-errno" -> Obuild_ast.Cstubs_ignore_errno
              | "return" | "return_errno" | "return-errno" -> Obuild_ast.Cstubs_return_errno
              | _ -> failwith (Printf.sprintf "Unknown errno policy: %s (expected: ignore_errno, return_errno)" value)
            in
            { cstubs with cstubs_errno = errno }
          | _ -> cstubs
        in
        loop cstubs' rest
      | _ -> loop cstubs rest
  in
  loop default_cstubs tokens

(** Parse per block *)
let parse_per_block args tokens =
  let per = {
    per_files = args;
    per_build_deps = [];
    per_oflags = [];
    per_pp = None;
  } in
  let rec loop per = function
    | [] -> per
    | t :: rest ->
      match t.tok with
      | KEY_VALUE (key, value) ->
        let per' = match String.lowercase_ascii key with
          | "builddepends" | "builddeps" | "build-deps" ->
            { per with per_build_deps = per.per_build_deps @ parse_dependencies value }
          | "oflags" ->
            { per with per_oflags = per.per_oflags @ parse_list value }
          | "pp" ->
            { per with per_pp = Some value }
          | _ -> per
        in
        loop per' rest
      | _ -> loop per rest
  in
  loop per tokens

(** Parse library block *)
let rec parse_library_block name tokens =
  let lib = {
    lib_name = name;
    lib_description = "";
    lib_modules = [];
    lib_pack = false;
    lib_syntax = false;
    lib_cstubs = None;
    lib_target = default_target_common;
    lib_subs = [];
  } in
  parse_library_tokens lib tokens

and parse_library_tokens lib tokens =
  match tokens with
  | [] -> lib
  | t :: rest ->
    match t.tok with
    | KEY_VALUE (key, value) ->
      let lib' = match String.lowercase_ascii key with
        | "modules" ->
          { lib with lib_modules = lib.lib_modules @ parse_list value }
        | "pack" ->
          { lib with lib_pack = String.lowercase_ascii value = "true" }
        | "syntax" ->
          { lib with lib_syntax = String.lowercase_ascii value = "true" }
        | "description" ->
          { lib with lib_description = value }
        | _ ->
          { lib with lib_target = parse_target_setting lib.lib_target key value }
      in
      parse_library_tokens lib' rest
    | BLOCK (name, args) ->
      (* Collect nested block *)
      let base_indent = t.indent in
      let nested, remaining = collect_nested rest base_indent in
      let lib' = match String.lowercase_ascii name with
        | "cstubs" ->
          { lib with lib_cstubs = Some (parse_cstubs_block nested) }
        | "per" ->
          let per = parse_per_block args nested in
          { lib with lib_target = { lib.lib_target with per = lib.lib_target.per @ [per] } }
        | "sub" | "sublib" | "library" ->
          let subname = match args with
            | [n] -> n
            | _ -> "unknown"
          in
          let sublib = parse_library_block subname nested in
          { lib with lib_subs = lib.lib_subs @ [sublib] }
        | _ -> lib
      in
      parse_library_tokens lib' remaining
    | _ -> parse_library_tokens lib rest

(** Collect tokens belonging to a nested block *)
and collect_nested tokens base_indent =
  let rec loop acc = function
    | [] -> (List.rev acc, [])
    | t :: rest as all ->
      if t.indent > base_indent then
        loop (t :: acc) rest
      else
        (List.rev acc, all)
  in
  loop [] tokens

(** Parse executable block *)
let parse_executable_block name tokens =
  let exe = {
    exe_name = name;
    exe_main = "";
    exe_target = default_target_common;
  } in
  let rec loop exe = function
    | [] -> exe
    | t :: rest ->
      match t.tok with
      | KEY_VALUE (key, value) ->
        let exe' = match String.lowercase_ascii key with
          | "main" | "main-is" ->
            { exe with exe_main = value }
          | _ ->
            { exe with exe_target = parse_target_setting exe.exe_target key value }
        in
        loop exe' rest
      | BLOCK (name, args) ->
        let base_indent = t.indent in
        let nested, remaining = collect_nested rest base_indent in
        let exe' = match String.lowercase_ascii name with
          | "per" ->
            let per = parse_per_block args nested in
            { exe with exe_target = { exe.exe_target with per = exe.exe_target.per @ [per] } }
          | _ -> exe
        in
        loop exe' remaining
      | _ -> loop exe rest
  in
  loop exe tokens

(** Parse test block *)
let parse_test_block name tokens =
  let test = {
    test_name = name;
    test_main = "";
    test_rundir = None;
    test_run_params = [];
    test_target = default_target_common;
  } in
  let rec loop test = function
    | [] -> test
    | t :: rest ->
      match t.tok with
      | KEY_VALUE (key, value) ->
        let test' = match String.lowercase_ascii key with
          | "main" | "main-is" ->
            { test with test_main = value }
          | "rundir" | "run-dir" ->
            { test with test_rundir = Some value }
          | "runparams" | "run-params" ->
            { test with test_run_params = parse_list value }
          | _ ->
            { test with test_target = parse_target_setting test.test_target key value }
        in
        loop test' rest
      | BLOCK (name, args) ->
        let base_indent = t.indent in
        let nested, remaining = collect_nested rest base_indent in
        let test' = match String.lowercase_ascii name with
          | "per" ->
            let per = parse_per_block args nested in
            { test with test_target = { test.test_target with per = test.test_target.per @ [per] } }
          | _ -> test
        in
        loop test' remaining
      | _ -> loop test rest
  in
  loop test tokens

(** Parse example block (same structure as executable) *)
let parse_example_block name tokens =
  let example = {
    example_name = name;
    example_main = "";
    example_target = default_target_common;
  } in
  let rec loop example = function
    | [] -> example
    | t :: rest ->
      match t.tok with
      | KEY_VALUE (key, value) ->
        let example' = match String.lowercase_ascii key with
          | "main" | "main-is" ->
            { example with example_main = value }
          | _ ->
            { example with example_target = parse_target_setting example.example_target key value }
        in
        loop example' rest
      | BLOCK (name, args) ->
        let base_indent = t.indent in
        let nested, remaining = collect_nested rest base_indent in
        let example' = match String.lowercase_ascii name with
          | "per" ->
            let per = parse_per_block args nested in
            { example with example_target = { example.example_target with per = example.example_target.per @ [per] } }
          | _ -> example
        in
        loop example' remaining
      | _ -> loop example rest
  in
  loop example tokens

(** Parse benchmark block *)
let parse_benchmark_block name tokens =
  let bench = {
    bench_name = name;
    bench_main = "";
    bench_target = default_target_common;
  } in
  let rec loop bench = function
    | [] -> bench
    | t :: rest ->
      match t.tok with
      | KEY_VALUE (key, value) ->
        let bench' = match String.lowercase_ascii key with
          | "main" | "main-is" ->
            { bench with bench_main = value }
          | _ ->
            { bench with bench_target = parse_target_setting bench.bench_target key value }
        in
        loop bench' rest
      | BLOCK (name, args) ->
        let base_indent = t.indent in
        let nested, remaining = collect_nested rest base_indent in
        let bench' = match String.lowercase_ascii name with
          | "per" ->
            let per = parse_per_block args nested in
            { bench with bench_target = { bench.bench_target with per = bench.bench_target.per @ [per] } }
          | _ -> bench
        in
        loop bench' remaining
      | _ -> loop bench rest
  in
  loop bench tokens

(** Parse flag block *)
let parse_flag_block name tokens =
  let flag = {
    flag_name = name;
    flag_description = "";
    flag_default = false;
  } in
  let rec loop flag = function
    | [] -> flag
    | t :: rest ->
      match t.tok with
      | KEY_VALUE (key, value) ->
        let flag' = match String.lowercase_ascii key with
          | "description" ->
            { flag with flag_description = value }
          | "default" ->
            { flag with flag_default = String.lowercase_ascii value = "true" }
          | _ -> flag
        in
        loop flag' rest
      | _ -> loop flag rest
  in
  loop flag tokens

(** Default empty project *)
let empty_project loc = {
  project_name = { value = ""; loc };
  project_version = { value = ""; loc };
  project_obuild_ver = { value = 0; loc };
  project_synopsis = None;
  project_description = None;
  project_license = None;
  project_license_file = None;
  project_homepage = None;
  project_authors = [];
  project_extra_srcs = [];
  project_extra_tools = [];
  project_configure_script = None;
  project_ocaml_ver = None;
  project_ocaml_extra_args = [];
  project_flags = [];
  project_libs = [];
  project_exes = [];
  project_tests = [];
  project_benchs = [];
  project_examples = [];
}

(** Parse top-level project *)
let parse_project tokens =
  let st = make_state tokens in
  let start_loc = (current st).loc in
  let proj = ref (empty_project start_loc) in

  while not (at_end st) do
    let t = current st in
    advance st;
    match t.tok with
    | KEY_VALUE (key, value) ->
      let p = !proj in
      proj := (match String.lowercase_ascii key with
        | "name" ->
          { p with project_name = { value; loc = t.loc } }
        | "version" ->
          { p with project_version = { value; loc = t.loc } }
        | "obuild-ver" ->
          { p with project_obuild_ver = { value = int_of_string value; loc = t.loc } }
        | "synopsis" ->
          { p with project_synopsis = Some value }
        | "description" ->
          { p with project_description = Some value }
        | "license" | "licence" ->
          { p with project_license = Some value }
        | "license-file" | "licence-file" ->
          { p with project_license_file = Some value }
        | "homepage" ->
          { p with project_homepage = Some value }
        | "authors" ->
          { p with project_authors = parse_list value }
        | "author" ->
          { p with project_authors = [value] }
        | "extra-srcs" ->
          { p with project_extra_srcs = p.project_extra_srcs @ parse_list value }
        | "tools" ->
          { p with project_extra_tools = p.project_extra_tools @ parse_list value }
        | "configure-script" ->
          { p with project_configure_script = Some value }
        | "ocamlversion" | "ocaml-version" ->
          { p with project_ocaml_ver = Some value }
        | "ocaml-extra-args" | "ocamlextraargs" ->
          { p with project_ocaml_extra_args = parse_words value }
        | _ -> p)
    | BLOCK (name, args) ->
      let block_tokens = collect_block st t.indent in
      let p = !proj in
      let block_name = match args with
        | [n] -> n
        | _ -> ""
      in
      proj := (match String.lowercase_ascii name with
        | "library" ->
          let lib = parse_library_block block_name block_tokens in
          { p with project_libs = p.project_libs @ [lib] }
        | "executable" ->
          let exe = parse_executable_block block_name block_tokens in
          { p with project_exes = p.project_exes @ [exe] }
        | "test" ->
          let test = parse_test_block block_name block_tokens in
          { p with project_tests = p.project_tests @ [test] }
        | "bench" | "benchmark" ->
          let bench = parse_benchmark_block block_name block_tokens in
          { p with project_benchs = p.project_benchs @ [bench] }
        | "example" ->
          let example = parse_example_block block_name block_tokens in
          { p with project_examples = p.project_examples @ [example] }
        | "flag" ->
          let flag = parse_flag_block block_name block_tokens in
          { p with project_flags = p.project_flags @ [flag] }
        | _ -> p)
    | _ -> ()
  done;
  !proj

(** Main parsing function: string -> project *)
let parse input =
  let tokens = Obuild_lexer.tokenize input in
  parse_project tokens

(** Parse from file *)
let parse_file path =
  let tokens = Obuild_lexer.tokenize_file path in
  parse_project tokens
