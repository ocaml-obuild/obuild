(** Pure AST types for .obuild files

    These types represent the parsed structure without any validation
    or file system checks. Validation happens in a separate pass.
*)

(** Location information for error reporting *)
type loc = Obuild_lexer.loc

(** A located value - pairs a value with its source location *)
type 'a located = {
  value: 'a;
  loc: loc;
}

(** Dependency with optional version constraint *)
type dependency = {
  dep_name: string;
  dep_constraint: string option;  (* e.g., ">= 1.0" *)
}

(** Extra dependency ordering: module A must be built before module B *)
type extra_dep = {
  before: string;  (* module name *)
  after: string;   (* module name *)
}

(** Per-file settings *)
type per_settings = {
  per_files: string list;         (* file patterns this applies to *)
  per_build_deps: dependency list;
  per_oflags: string list;
  per_pp: string option;
}

(** Cstubs functor description: "Bindings.Types -> Types_gen" *)
type cstubs_desc = {
  cstubs_functor: string;   (* e.g., "Bindings.Types" *)
  cstubs_instance: string;  (* e.g., "Types_gen" *)
}

(** Cstubs concurrency policy *)
type cstubs_concurrency =
  | Cstubs_sequential       (* Default: no special concurrency support *)
  | Cstubs_unlocked         (* Release runtime lock during C calls *)
  | Cstubs_lwt_jobs         (* Lwt jobs-based concurrency *)
  | Cstubs_lwt_preemptive   (* Lwt preemptive threading *)

(** Cstubs errno policy *)
type cstubs_errno =
  | Cstubs_ignore_errno     (* Default: errno not accessed *)
  | Cstubs_return_errno     (* Functions return (retval, errno) pairs *)

(** Cstubs configuration block *)
type cstubs = {
  cstubs_external_lib_name: string;
  cstubs_type_desc: cstubs_desc option;
  cstubs_func_desc: cstubs_desc option;
  cstubs_generated_types: string;       (* default: "Types_generated" *)
  cstubs_generated_entry: string;       (* default: "C" *)
  cstubs_headers: string list;
  cstubs_concurrency: cstubs_concurrency; (* default: Cstubs_sequential *)
  cstubs_errno: cstubs_errno;             (* default: Cstubs_ignore_errno *)
}

(** Stdlib choice *)
type stdlib =
  | Stdlib_None
  | Stdlib_Standard
  | Stdlib_Core

(** Runtime boolean - can be constant or a flag variable *)
type runtime_bool =
  | Bool_const of bool
  | Bool_var of string

(** C-related settings (shared by all target types) *)
type c_settings = {
  c_dir: string option;
  c_sources: string list;
  c_flags: string list;
  c_libs: string list;
  c_lib_paths: string list;
  c_pkgs: dependency list;
}

(** OCaml-related settings (shared by all target types) *)
type ocaml_settings = {
  src_dir: string list;
  build_deps: dependency list;
  pp: string option;
  extra_deps: extra_dep list;
  oflags: string list;
  stdlib: stdlib option;
}

(** Common target settings *)
type target_common = {
  buildable: runtime_bool;
  installable: runtime_bool;
  ocaml: ocaml_settings;
  c: c_settings;
  per: per_settings list;
}

(** Library-specific settings *)
type library = {
  lib_name: string;
  lib_description: string;
  lib_modules: string list;
  lib_pack: bool;
  lib_syntax: bool;
  lib_cstubs: cstubs option;
  lib_target: target_common;
  lib_subs: library list;         (* sub-libraries *)
}

(** Executable-specific settings *)
type executable = {
  exe_name: string;
  exe_main: string;               (* main-is *)
  exe_target: target_common;
}

(** Test settings (similar to executable) *)
type test = {
  test_name: string;
  test_main: string;
  test_rundir: string option;
  test_run_params: string list;
  test_target: target_common;
}

(** Benchmark settings *)
type benchmark = {
  bench_name: string;
  bench_main: string;
  bench_target: target_common;
}

(** Example settings *)
type example = {
  example_name: string;
  example_main: string;
  example_target: target_common;
}

(** Flag definition *)
type flag = {
  flag_name: string;
  flag_description: string;
  flag_default: bool;
}

(** Top-level project AST *)
type project = {
  (* Required fields *)
  project_name: string located;
  project_version: string located;
  project_obuild_ver: int located;

  (* Optional metadata *)
  project_synopsis: string option;
  project_description: string option;
  project_license: string option;
  project_license_file: string option;
  project_homepage: string option;
  project_authors: string list;

  (* Build configuration *)
  project_extra_srcs: string list;
  project_extra_tools: string list;
  project_configure_script: string option;
  project_ocaml_ver: string option;      (* version constraint expr *)
  project_ocaml_extra_args: string list;

  (* Flags and targets *)
  project_flags: flag list;
  project_libs: library list;
  project_exes: executable list;
  project_tests: test list;
  project_benchs: benchmark list;
  project_examples: example list;
}

(** Default values *)

let default_c_settings = {
  c_dir = None;
  c_sources = [];
  c_flags = [];
  c_libs = [];
  c_lib_paths = [];
  c_pkgs = [];
}

let default_ocaml_settings = {
  src_dir = [];
  build_deps = [];
  pp = None;
  extra_deps = [];
  oflags = [];
  stdlib = None;
}

let default_target_common = {
  buildable = Bool_const true;
  installable = Bool_const true;
  ocaml = default_ocaml_settings;
  c = default_c_settings;
  per = [];
}

let default_cstubs = {
  cstubs_external_lib_name = "";
  cstubs_type_desc = None;
  cstubs_func_desc = None;
  cstubs_generated_types = "Types_generated";
  cstubs_generated_entry = "C";
  cstubs_headers = [];
  cstubs_concurrency = Cstubs_sequential;
  cstubs_errno = Cstubs_ignore_errno;
}
