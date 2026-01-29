(** Validation and transformation from AST to Project types

    Converts the pure AST from the parser into the existing internal types (Project.t, Target.t,
    etc.) with validation. *)

open Filepath
open Obuild_lexer
open Obuild_ast
open Location

exception Validation_error of loc * string
(** Validation error with location *)

(** Raise a validation error *)
let error loc msg = raise (Validation_error (loc, msg))

(** Format location for error messages *)
let loc_to_string loc = Printf.sprintf "%d:%d" loc.line loc.col

(** Format validation error *)
let error_to_string loc msg = Printf.sprintf "%s: %s" (loc_to_string loc) msg

(* ============================================================ *)
(* Helper conversions *)
(* ============================================================ *)

(** Convert AST stdlib to Target stdlib *)
let convert_stdlib = function
  | Stdlib_None -> Target.Stdlib_None
  | Stdlib_Standard -> Target.Stdlib_Standard
  | Stdlib_Core -> Target.Stdlib_Core

(** Convert AST runtime_bool to Target runtime_bool *)
let convert_runtime_bool = function
  | Bool_const b -> Target.BoolConst b
  | Bool_var v -> Target.BoolVariable v

(** Convert AST dependency to Dependencies.dependency *)
let convert_dependency (dep : dependency) : Dependencies.dependency =
  let libname = Libname.of_string dep.dep_name in
  let constraint_expr =
    match dep.dep_constraint with
    | None -> None
    | Some s -> Expr.parse dep.dep_name s
  in
  (libname, constraint_expr)

(** Convert AST dependency to C dependency (just string, constraint) *)
let convert_cdependency (dep : dependency) : Dependencies.cdependency =
  let constraint_expr =
    match dep.dep_constraint with
    | None -> None
    | Some s -> Expr.parse dep.dep_name s
  in
  (dep.dep_name, constraint_expr)

(** Convert AST extra_dep to (Hier.t * Hier.t) *)
let convert_extra_dep (ed : extra_dep) : Hier.t * Hier.t =
  (Hier.of_string ed.before, Hier.of_string ed.after)

(** Convert module name string to Hier.t *)
let module_name_to_hier s = Hier.make [ Modname.wrap (Compat.string_capitalize s) ]

(* ============================================================ *)
(* Convert C settings *)
(* ============================================================ *)

let convert_c_settings ?(default_cdir=Filepath.current_dir) (c : c_settings) : Target.target_cbits =
  {
    Target.target_cdir =
      (match c.c_dir with
      | None -> default_cdir
      | Some s -> fp s);
    target_csources = List.map fn c.c_sources;
    target_cflags = c.c_flags;
    target_clibs = c.c_libs;
    target_clibpaths = List.map fp c.c_lib_paths;
    target_cpkgs = List.map convert_cdependency c.c_pkgs;
  }

(* ============================================================ *)
(* Convert OCaml settings *)
(* ============================================================ *)

let convert_ocaml_settings (o : ocaml_settings) : Target.target_obits =
  {
    Target.target_srcdir =
      (match o.src_dir with
      | [] -> [ Filepath.current_dir ]
      | dirs -> List.map fp dirs);
    target_builddeps = List.map convert_dependency o.build_deps;
    target_oflags = o.oflags;
    target_pp =
      (match o.pp with
      | None -> None
      | Some s -> Some (Pp.Type.of_string s));
    target_extradeps = List.map convert_extra_dep o.extra_deps;
    target_stdlib =
      (match o.stdlib with
      | None -> Target.Stdlib_Standard
      | Some s -> convert_stdlib s);
  }

(* ============================================================ *)
(* Convert cstubs *)
(* ============================================================ *)

let convert_cstubs_desc (desc : cstubs_desc) : Target.cstubs_description =
  {
    Target.cstubs_functor = Hier.of_string desc.cstubs_functor;
    cstubs_instance = desc.cstubs_instance;
  }

let convert_cstubs_concurrency = function
  | Cstubs_sequential -> Target.Cstubs_sequential
  | Cstubs_unlocked -> Target.Cstubs_unlocked
  | Cstubs_lwt_jobs -> Target.Cstubs_lwt_jobs
  | Cstubs_lwt_preemptive -> Target.Cstubs_lwt_preemptive

let convert_cstubs_errno = function
  | Cstubs_ignore_errno -> Target.Cstubs_ignore_errno
  | Cstubs_return_errno -> Target.Cstubs_return_errno

let convert_cstubs (cs : cstubs) : Target.target_cstubs =
  {
    Target.cstubs_external_library_name = cs.cstubs_external_lib_name;
    cstubs_type_description = Compat.Option.map convert_cstubs_desc cs.cstubs_type_desc;
    cstubs_function_description = Compat.Option.map convert_cstubs_desc cs.cstubs_func_desc;
    cstubs_generated_types = cs.cstubs_generated_types;
    cstubs_generated_entry_point = cs.cstubs_generated_entry;
    cstubs_headers = cs.cstubs_headers;
    cstubs_concurrency = convert_cstubs_concurrency cs.cstubs_concurrency;
    cstubs_errno = convert_cstubs_errno cs.cstubs_errno;
  }

(* ============================================================ *)
(* Convert generator types *)
(* ============================================================ *)

let convert_generator (gen : Obuild_ast.generator) : Project.Generator.t =
  {
    Project.Generator.name = gen.gen_name;
    suffix = gen.gen_suffix;
    command = gen.gen_command;
    outputs = gen.gen_outputs;
    module_name = gen.gen_module_name;
  }

let convert_generate_block (gen : Obuild_ast.generate_block) : Target.target_generate =
  {
    Target.generate_module = Hier.of_string gen.generate_module;
    generate_from = List.map fp gen.generate_from;
    generate_using = gen.generate_using;
    generate_args = gen.generate_args;
  }

(* ============================================================ *)
(* Convert per block (target_extra) *)
(* ============================================================ *)

let convert_per (per : per_settings) : Target.target_extra =
  {
    Target.target_extra_objects = per.per_files;
    target_extra_builddeps = List.map convert_dependency per.per_build_deps;
    target_extra_oflags = per.per_oflags;
    target_extra_cflags = [];
    (* per blocks don't have cflags in AST *)
    target_extra_pp = Compat.Option.map Pp.Type.of_string per.per_pp;
  }

(* ============================================================ *)
(* Convert target_common to target *)
(* ============================================================ *)

let convert_target_common name typ ?cstubs (tc : target_common) : Target.target =
  (* Use src-dir as default for c-dir if c-dir is not specified *)
  let default_cdir = match tc.ocaml.src_dir with
    | [] -> Filepath.current_dir
    | dir :: _ -> fp dir
  in
  {
    Target.target_name = name;
    target_type = typ;
    target_cbits = convert_c_settings ~default_cdir tc.c;
    target_obits = convert_ocaml_settings tc.ocaml;
    target_cstubs = Compat.Option.map convert_cstubs cstubs;
    target_generates = List.map convert_generate_block tc.generates;
    target_extras = List.map convert_per tc.per;
    target_buildable = convert_runtime_bool tc.buildable;
    target_installable = convert_runtime_bool tc.installable;
  }

(* ============================================================ *)
(* Convert library *)
(* ============================================================ *)

let rec convert_library (lib : library) : Project.Library.t =
  let libname = Libname.of_string lib.lib_name in
  let target_name = Target.Name.Lib libname in
  let target =
    convert_target_common target_name Target.Typ.Lib ?cstubs:lib.lib_cstubs lib.lib_target
  in

  (* Auto-add cstubs generated modules if present *)
  let base_modules = List.map module_name_to_hier lib.lib_modules in
  let modules =
    match lib.lib_cstubs with
    | None -> base_modules
    | Some cs ->
        (* All three modules are derived from cstubs config:
         - <lib>_generated: the FOREIGN implementation
         - generated-types: type bindings (e.g., Types_generated)
         - generated-entry-point: entry module (e.g., C) *)
        let foreign_name = cs.cstubs_external_lib_name ^ "_generated" in
        let generated_modules =
          [
            Hier.of_string (Compat.string_capitalize foreign_name);
            Hier.of_string (Compat.string_capitalize cs.cstubs_generated_types);
            Hier.of_string (Compat.string_capitalize cs.cstubs_generated_entry);
          ]
        in
        (* Add any that aren't already in the list *)
        List.fold_left
          (fun acc m -> if List.mem m acc then acc else m :: acc)
          base_modules generated_modules
  in

  {
    Project.Library.name = libname;
    description = lib.lib_description;
    target;
    modules;
    pack = lib.lib_pack;
    syntax = lib.lib_syntax;
    subs = List.map (convert_sublibrary libname) lib.lib_subs;
  }

and convert_sublibrary parent_name (lib : library) : Project.Library.t =
  let libname = Libname.append parent_name lib.lib_name in
  let target_name = Target.Name.Lib libname in
  let target =
    convert_target_common target_name Target.Typ.Lib ?cstubs:lib.lib_cstubs lib.lib_target
  in

  (* Auto-add cstubs generated modules if present *)
  let base_modules = List.map module_name_to_hier lib.lib_modules in
  let modules =
    match lib.lib_cstubs with
    | None -> base_modules
    | Some cs ->
        let foreign_name = cs.cstubs_external_lib_name ^ "_generated" in
        let generated_modules =
          [
            Hier.of_string (Compat.string_capitalize foreign_name);
            Hier.of_string (Compat.string_capitalize cs.cstubs_generated_types);
            Hier.of_string (Compat.string_capitalize cs.cstubs_generated_entry);
          ]
        in
        List.fold_left
          (fun acc m -> if List.mem m acc then acc else m :: acc)
          base_modules generated_modules
  in

  {
    Project.Library.name = libname;
    description = lib.lib_description;
    target;
    modules;
    pack = lib.lib_pack;
    syntax = lib.lib_syntax;
    subs = List.map (convert_sublibrary libname) lib.lib_subs;
  }

(* ============================================================ *)
(* Convert executable *)
(* ============================================================ *)

let convert_executable (exe : executable) : Project.Executable.t =
  let target_name = Target.Name.Exe exe.exe_name in
  let target = convert_target_common target_name Target.Typ.Exe exe.exe_target in
  { Project.Executable.name = exe.exe_name; main = fn exe.exe_main; target }

(* ============================================================ *)
(* Convert test *)
(* ============================================================ *)

let convert_test (t : test) : Project.Test.t =
  let target_name = Target.Name.Test t.test_name in
  let target = convert_target_common target_name Target.Typ.Test t.test_target in
  Project.Test.make
    ~name:t.test_name
    ~main:(fn t.test_main)
    ~target
    ~rundir:(Compat.Option.map fp t.test_rundir)
    ~runopt:t.test_run_params

(* ============================================================ *)
(* Convert benchmark *)
(* ============================================================ *)

let convert_benchmark (b : benchmark) : Project.Bench.t =
  let target_name = Target.Name.Bench b.bench_name in
  let target = convert_target_common target_name Target.Typ.Bench b.bench_target in
  Project.Bench.make ~name:b.bench_name ~main:(fn b.bench_main) ~target

(* ============================================================ *)
(* Convert example *)
(* ============================================================ *)

let convert_example (ex : example) : Project.Example.t =
  let target_name = Target.Name.Example ex.example_name in
  let target = convert_target_common target_name Target.Typ.Test ex.example_target in
  Project.Example.make ~name:ex.example_name ~main:(fn ex.example_main) ~target

(* ============================================================ *)
(* Convert flag *)
(* ============================================================ *)

let convert_flag (f : flag) : Project.Flag.t =
  {
    Project.Flag.name = f.flag_name;
    description = f.flag_description;
    default = (if f.flag_default then Some true else Some false);
  }

(* ============================================================ *)
(* Validation *)
(* ============================================================ *)

(** Validate required project fields *)
let validate_required_fields (proj : project) =
  if proj.project_name.value = "" then
    error proj.project_name.loc "Missing required field: name";
  if proj.project_version.value = "" then
    error proj.project_version.loc "Missing required field: version";
  if proj.project_obuild_ver.value = 0 then
    error proj.project_obuild_ver.loc "Missing required field: obuild-ver";
  if proj.project_obuild_ver.value > 1 then
    error proj.project_obuild_ver.loc
      (Printf.sprintf "Unsupported obuild version: %d (max supported: 1)"
         proj.project_obuild_ver.value)

(** Validate library has modules *)
let rec validate_library (lib : library) =
  if lib.lib_modules = [] then
    failwith (Printf.sprintf "Library '%s' has no modules" lib.lib_name);
  List.iter validate_library lib.lib_subs

(** Validate executable has main *)
let validate_executable (exe : executable) =
  if exe.exe_main = "" then
    failwith (Printf.sprintf "Executable '%s' has no main-is" exe.exe_name)

(** Validate test has main *)
let validate_test (t : test) =
  if t.test_main = "" then
    failwith (Printf.sprintf "Test '%s' has no main-is" t.test_name)

(** Validate all targets *)
let validate_targets (proj : project) =
  List.iter validate_library proj.project_libs;
  List.iter validate_executable proj.project_exes;
  List.iter validate_test proj.project_tests

(* ============================================================ *)
(* Main conversion function *)
(* ============================================================ *)

(** Convert and validate AST project to Project.t *)
let convert (proj : project) : Project.t =
  (* Validate first *)
  validate_required_fields proj;
  validate_targets proj;

  (* Convert *)
  {
    Project.name = proj.project_name.value;
    version = proj.project_version.value;
    synopsis = Compat.Option.value ~default:"" proj.project_synopsis;
    description = Compat.Option.value ~default:"" proj.project_description;
    license = Compat.Option.value ~default:"" proj.project_license;
    license_file = Compat.Option.map fp proj.project_license_file;
    authors = proj.project_authors;
    obuild_ver = proj.project_obuild_ver.value;
    ocaml_ver = Compat.Option.bind proj.project_ocaml_ver (fun s -> Expr.parse "ocaml-version" s);
    homepage = Compat.Option.value ~default:"" proj.project_homepage;
    flags = List.map convert_flag proj.project_flags;
    generators = List.map convert_generator proj.project_generators;
    libs = List.map convert_library proj.project_libs;
    exes = List.map convert_executable proj.project_exes;
    tests = List.map convert_test proj.project_tests;
    benchs = List.map convert_benchmark proj.project_benchs;
    examples = List.map convert_example proj.project_examples;
    extra_srcs = List.map fp proj.project_extra_srcs;
    extra_tools = List.map fn proj.project_extra_tools;
    configure_script = Compat.Option.map fp proj.project_configure_script;
    ocaml_extra_args =
      (match proj.project_ocaml_extra_args with
      | [] -> None
      | args -> Some args);
  }

(** Parse and convert from string *)
let parse_and_convert input : Project.t =
  let ast = Obuild_parser.parse input in
  convert ast

(** Parse and convert from file *)
let parse_and_convert_file path : Project.t =
  let ast = Obuild_parser.parse_file path in
  convert ast
