(** Project configuration file (.obuild) types and utilities

    This module defines the types for representing obuild project files. Parsing is handled by
    obuild_parser and obuild_validate modules. Use Project_read.read() to parse project files. *)

open Fugue
open Filepath
open Printf
open Target

exception NoConfFile
exception MultipleConfFiles
exception InvalidConfFile of string
exception MissingField of string
exception UnknownDependencyName of string
exception UnsupportedFutureVersion of int
exception ModuleDoesntExist of target * Hier.t
exception ModuleListEmpty of Libname.t
exception FileDoesntExist of target * filename
exception LicenseFileDoesntExist of filepath
exception BlockSectionAsValue of string
exception ExecutableWithNoMain of string
exception UnknownStdlib of string
exception UnknownExtraDepFormat of string
exception UnknownFlag of string
exception BadOcamlVersion of (string * Expr.t)
exception LibraryNotFound of Libname.t
exception ExecutableNotFound of string
exception BenchNotFound of string
exception TestNotFound of string
exception ExampleNotFound of string

module Library = struct
  type t = {
    name : Libname.t;
    description : string;
    target : target;
    modules : Hier.t list;
    pack : bool;
    syntax : bool;
    subs : t list;
  }

  let make name =
    {
      name;
      description = "";
      modules = [];
      pack = false;
      syntax = false;
      target = new_target (Name.Lib name) Typ.Lib true true;
      subs = [];
    }

  let make_prefix libname subname = make (Libname.append libname subname)
  let make_from_string libname = make (Libname.of_string libname)
  let to_target obj = obj.target
  let rec to_targets lib = lib.target :: List.concat (List.map to_targets lib.subs)
  let rec flatten lib : t list = lib :: List.concat (List.map flatten lib.subs)

  let find libs name =
    try List.find (fun l -> l.name = name) (List.concat (List.map flatten libs))
    with Not_found -> raise (LibraryNotFound name)

  let check_modules_not_empty lib = if lib.modules = [] then raise (ModuleListEmpty lib.name)

  let rec show add show_target section lib =
    add "\n";
    add (sprintf "%slibrary %s\n" section (Libname.to_string lib.name));
    let iStr = section ^ "  " in
    add (sprintf "%smodules: %s\n" iStr (Utils.showList "," Hier.to_string lib.modules));
    if lib.pack then add (sprintf "%spack: %b\n" iStr lib.pack);
    if lib.syntax then add (sprintf "%ssyntax: %b\n" iStr lib.syntax);
    if lib.pack then add (sprintf "%spack: %b\n" iStr lib.pack);
    show_target iStr lib.target;
    List.iter (fun sub -> show add show_target iStr sub) lib.subs
end

module Executable = struct
  type t = {
    name : string;
    main : filename;
    target : target;
  }

  let make name = { name; main = empty_fn; target = new_target (Name.Exe name) Typ.Exe true true }
  let to_target obj = obj.target

  let find exes name =
    try List.find (fun e -> e.name = name) exes with Not_found -> raise (ExecutableNotFound name)
end

module Test = struct
  type test_type = ExitCode

  type t = {
    name : string;
    main : filename;
    target : target;
    rundir : filepath option;
    runopt : string list;
    type_ : test_type;
  }

  let make ~name ~main ~target ~rundir ~runopt =
    (* For tests, buildable defaults to CLI option "build-tests" *)
    let buildable =
      match target.target_buildable with
      | BoolConst true -> BoolConst (Gconf.get_target_option "build-tests")
      | other -> other
    in
    {
      name;
      main;
      target = { target with target_buildable = buildable; target_installable = BoolConst false };
      rundir;
      runopt;
      type_ = ExitCode;
    }

  let to_target obj = obj.target

  let find tests name =
    try List.find (fun b -> b.name = name) tests with Not_found -> raise (TestNotFound name)
end

module Bench = struct
  type t = {
    name : string;
    main : filename;
    target : target;
  }

  let make ~name ~main ~target =
    (* For benchmarks, buildable defaults to CLI option "build-benchs" *)
    let buildable =
      match target.target_buildable with
      | BoolConst true -> BoolConst (Gconf.get_target_option "build-benchs")
      | other -> other
    in
    {
      name;
      main;
      target = { target with target_buildable = buildable; target_installable = BoolConst false };
    }

  let to_target obj = obj.target

  let find benchs name =
    try List.find (fun b -> b.name = name) benchs with Not_found -> raise (BenchNotFound name)
end

module Example = struct
  type t = {
    name : string;
    main : filename;
    target : target;
  }

  let to_target obj = obj.target

  let make ~name ~main ~target =
    (* For examples, buildable defaults to CLI option "build-examples" *)
    let buildable =
      match target.target_buildable with
      | BoolConst true -> BoolConst (Gconf.get_target_option "build-examples")
      | other -> other
    in
    {
      name;
      main;
      target = { target with target_buildable = buildable; target_installable = BoolConst false };
    }

  let find examples name =
    try List.find (fun b -> b.name = name) examples with Not_found -> raise (ExampleNotFound name)
end

module Flag = struct
  type t = {
    name : string;
    description : string;
    default : bool option;
  }
end

module Generator = struct
  type t = {
    name : string;                    (** Generator name for reference *)
    suffix : string option;           (** File extension for automatic detection (e.g., "mly") *)
    command : string;                 (** Command template with variables: ${src}, ${dest}, ${base}, ${sources} *)
    outputs : string list;            (** Output file patterns *)
    module_name : string option;      (** Module name pattern if different from base *)
  }

  let make name =
    {
      name;
      suffix = None;
      command = "";
      outputs = [];
      module_name = None;
    }
end

type t = {
  name : string;
  version : string;
  synopsis : string;
  description : string;
  license : string;
  license_file : filepath option;
  authors : string list;
  obuild_ver : int;
  ocaml_ver : Expr.t option;
  homepage : string;
  flags : Flag.t list;
  generators : Generator.t list;
  libs : Library.t list;
  exes : Executable.t list;
  tests : Test.t list;
  benchs : Bench.t list;
  examples : Example.t list;
  extra_srcs : filepath list;
  extra_tools : filename list;
  configure_script : filepath option;
  ocaml_extra_args : string list option;
}

let make =
  {
    name = "";
    version = "";
    synopsis = "";
    description = "";
    license = "";
    license_file = None;
    authors = [];
    obuild_ver = 0;
    ocaml_ver = None;
    homepage = "";
    extra_tools = [];
    flags = [];
    generators = [];
    libs = [];
    exes = [];
    tests = [];
    benchs = [];
    examples = [];
    extra_srcs = [];
    configure_script = None;
    ocaml_extra_args = None;
  }

let findPath () =
  let ents = List.fast_sort String.compare (Array.to_list (Sys.readdir ".")) in
  match
    List.find_all
      (fun ent -> (not (String_utils.startswith "." ent)) && String_utils.endswith ".obuild" ent)
      ents
  with
  | [] -> raise NoConfFile
  | [ x ] -> fp x
  | _ -> raise MultipleConfFiles

let digest () =
  let path = findPath () in
  Digest.to_hex (Digest.file (fp_to_string path))

(** Helper: Validate that files exist in target source directories *)
let check_files_exists target names =
  let srcdir = target.target_obits.target_srcdir in
  List.iter (fun n -> ignore (Utils.find_in_paths srcdir n)) names

(** Helper: Validate that modules exist in target source directories Skips modules that will be
    auto-generated by cstubs *)
let check_modules_exists target modules =
  let srcdir = target.target_obits.target_srcdir in
  (* Get list of cstubs-generated modules to skip - all three are generated:
     - <lib>_generated: FOREIGN implementation
     - generated-types: type bindings
     - generated-entry-point: entry module *)
  let cstubs_generated_modules =
    match target.target_cstubs with
    | Some cstubs ->
        let foreign_name =
          Compat.string_capitalize (cstubs.cstubs_external_library_name ^ "_generated")
        in
        let types_name = Compat.string_capitalize cstubs.cstubs_generated_types in
        let entry_name = Compat.string_capitalize cstubs.cstubs_generated_entry_point in
        [ Hier.of_string foreign_name; Hier.of_string types_name; Hier.of_string entry_name ]
    | None -> []
  in
  List.iter
    (fun m ->
      (* Skip validation for cstubs-generated modules *)
      if not (List.mem m cstubs_generated_modules) then
        try
          ignore (Hier.get_file_entry m srcdir)
        with Not_found -> raise (ModuleDoesntExist (target, m)))
    modules

(** Validate project configuration

    Checks for required fields, file existence, module existence, and OCaml version compatibility.
*)
let check proj =
  if proj.name = "" then raise (MissingField "name");
  if proj.version = "" then raise (MissingField "version");
  if proj.obuild_ver = 0 then raise (MissingField "obuild-ver");
  if proj.obuild_ver > 1 then raise (UnsupportedFutureVersion proj.obuild_ver);

  maybe_unit
    (fun x -> if not (Filesystem.exists x) then raise (LicenseFileDoesntExist x))
    proj.license_file;
  maybe_unit
    (fun x ->
      let ocaml_ver = Hashtbl.find (Prog.get_ocaml_config ()) "version" in
      if not (Expr.eval ocaml_ver x) then raise (BadOcamlVersion (ocaml_ver, x)))
    proj.ocaml_ver;

  (* check sublibs in libs *)
  List.iter
    (fun rootlib ->
      Library.check_modules_not_empty rootlib;
      let sublibs = Library.flatten rootlib in
      List.iter
        (fun lib ->
          Library.check_modules_not_empty lib;
          check_modules_exists lib.Library.target lib.Library.modules)
        sublibs)
    proj.libs;

  List.iter
    (fun exe ->
      if fn_to_string exe.Executable.main = "" then
        raise (ExecutableWithNoMain exe.Executable.name);
      check_files_exists exe.Executable.target [ exe.Executable.main ])
    proj.exes;
  ()

let write file proj =
  Utils.generateFile file (fun add ->
      let add_string k s = if s <> "" then add (sprintf "%s: %s\n" k s) in

      add (sprintf "name: %s\n" proj.name);
      add (sprintf "version: %s\n" proj.version);
      add_string "synopsis" proj.synopsis;
      add_string "description" proj.description;
      add_string "license" proj.license;
      add_string "homepage" proj.homepage;
      maybe () (fun x -> add_string "license-file" (fp_to_string x)) proj.license_file;
      add_string "authors" (Utils.showList ", " id proj.authors);
      add (sprintf "obuild-ver: %d\n" proj.obuild_ver);
      maybe () (fun x -> add_string "ocaml-version" (Expr.to_string x)) proj.ocaml_ver;
      maybe () (fun x -> add_string "ocaml-extra-args" (String.concat " " x)) proj.ocaml_extra_args;

      let show_target iStr target =
        let obits = target.target_obits in
        let cbits = target.target_cbits in
        add
          (sprintf "%ssrc-dir: %s\n" iStr
             (String.concat "," (List.map fp_to_string obits.target_srcdir)));
        add_string (iStr ^ "build-deps")
          (Utils.showList ", " (fun (l, _) -> Libname.to_string l) obits.target_builddeps);
        add_string (iStr ^ "oflags") (Utils.showList " " id obits.target_oflags);
        add_string (iStr ^ "pp") (maybe "" (fun ppty -> Pp.Type.to_string ppty) obits.target_pp);

        add (sprintf "%sc-dir: %s\n" iStr (fp_to_string cbits.target_cdir));
        add_string (iStr ^ "c-sources") (Utils.showList ", " fn_to_string cbits.target_csources);
        add_string (iStr ^ "c-flags") (Utils.showList " " id cbits.target_cflags);
        add_string (iStr ^ "c-libs") (Utils.showList "," id cbits.target_clibs);
        add_string (iStr ^ "c-libpaths") (Utils.showList "," fp_to_string cbits.target_clibpaths);
        add_string (iStr ^ "c-pkgs") (Utils.showList ", " (fun (l, _) -> l) cbits.target_cpkgs)
      in
      List.iter (Library.show add show_target "") proj.libs;
      List.iter
        (fun exe ->
          add "\n";
          add (sprintf "executable %s\n" exe.Executable.name);
          add (sprintf "  main: %s\n" (fn_to_string exe.Executable.main));
          show_target "  " exe.Executable.target;
          ())
        proj.exes)

let get_all_targets projFile =
  List.concat (List.map Library.to_targets projFile.libs)
  @ List.map Executable.to_target projFile.exes
  @ List.map Test.to_target projFile.tests
  @ List.map Bench.to_target projFile.benchs
  @ List.map Example.to_target projFile.examples

let get_all_targets_filter projFile f =
  List.filter (fun target -> f target) (get_all_targets projFile)

let get_val_const_or_var user_flags = function
  | BoolConst t -> t
  | BoolVariable v -> ( try List.assoc v user_flags with Not_found -> raise (UnknownFlag v))

let get_all_buildable_targets proj_file user_flags =
  get_all_targets_filter proj_file (fun target ->
      get_val_const_or_var user_flags target.target_buildable)

let get_all_installable_targets proj_file user_flags =
  get_all_targets_filter proj_file (fun target ->
      let install = get_val_const_or_var user_flags target.target_installable in
      let build = get_val_const_or_var user_flags target.target_buildable in
      Printf.printf "target %s install %b build %b\n"
        (Target.Name.to_string target.target_name)
        install build;
      install)

let find_lib proj_file name = Library.find proj_file.libs name
let find_exe proj_file name = Executable.find proj_file.exes name
let find_test proj_file name = Test.find proj_file.tests name
let find_bench proj_file name = Bench.find proj_file.benchs name
let find_example proj_file name = Example.find proj_file.examples name
