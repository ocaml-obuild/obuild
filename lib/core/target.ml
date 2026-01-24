open Filepath
open Fugue
open Types
open Dependencies

module Typ = struct
  type t =
    | Lib
    | Exe
    | Test
    | Bench

  let is_lib t = t = Lib
end

exception TargetNameNoType of string
exception TargetUnknownType of string * string
exception TargetNotRecognized of string

module Name = struct
  type t =
    | Lib of Libname.t
    | Exe of string
    | Test of string
    | Bench of string
    | Example of string

  let to_string = function
    | Exe e -> "exe-" ^ e
    | Bench e -> "bench-" ^ e
    | Test e -> "test-" ^ e
    | Example e -> "example-" ^ e
    | Lib l -> "lib-" ^ Libname.to_string l

  let of_string name =
    match String_utils.split ~limit:2 '-' name with
    | [ "exe"; n ] -> Exe n
    | [ "lib"; n ] -> Lib (Libname.of_string n)
    | [ "test"; n ] -> Test n
    | [ "bench"; n ] -> Bench n
    | [ "example"; n ] -> Example n
    | [ prefix; n ] -> raise (TargetUnknownType (prefix, n))
    | [ _ ] -> raise (TargetNameNoType name)
    | _ -> raise (TargetNotRecognized name)

  let to_dirname = function
    | Exe e | Bench e | Test e | Example e -> fn e
    | Lib l -> fn ("lib-" ^ Libname.to_string l)

  let get_clibname = function
    | Exe e -> "stubs_" ^ e
    | Bench e -> "stubs_" ^ e
    | Test e -> "stubs_" ^ e
    | Example e -> "stubs_" ^ e
    | Lib l -> "stubs_" ^ list_last (Libname.to_string_nodes l)

  (* get the core name of the final object representing the object
   * for an executable/test/bench it will be the name of the executable apart from the extension
   * for a test it will be the name of the library created (.cmxa/.cma) apart from the extension
   *)
  let get_dest_name = function
    | Exe e -> e
    | Bench e -> "bench-" ^ e
    | Test e -> "test-" ^ e
    | Example e -> "example-" ^ e
    | Lib l -> String.concat "_" (Libname.to_string_nodes l)
end

type target_stdlib =
  | Stdlib_None
  | Stdlib_Standard
  | Stdlib_Core

type runtime_bool =
  | BoolConst of bool
  | BoolVariable of string

let runtime_def v = BoolConst v

type target_cbits = {
  target_cdir : filepath;
  target_csources : filename list;
  target_cflags : string list (* CFLAGS *);
  target_clibs : string list;
  target_clibpaths : filepath list;
  target_cpkgs : cdependency list (* pkg-config name *);
}

type target_obits = {
  target_srcdir : filepath list;
  target_builddeps : dependency list;
  target_oflags : string list;
  target_pp : Pp.Type.t option;
  target_extradeps : (Hier.t * Hier.t) list;
  target_stdlib : target_stdlib;
}

type target_extra = {
  target_extra_objects : string list; (* targets of those extra settings *)
  target_extra_builddeps : dependency list;
  target_extra_oflags : string list;
  target_extra_cflags : string list;
  target_extra_pp : Pp.Type.t option;
}

(* Ctypes.cstubs support: pair of functor module -> generated instance name *)
type cstubs_description = {
  cstubs_functor : Hier.t; (* User's functor module, e.g., Bindings.Types *)
  cstubs_instance : string; (* Generated instance name, e.g., Types_gen *)
}

(* Ctypes.cstubs concurrency policy *)
type cstubs_concurrency =
  | Cstubs_sequential (* Default: no special concurrency support *)
  | Cstubs_unlocked (* Release runtime lock during C calls *)
  | Cstubs_lwt_jobs (* Lwt jobs-based concurrency *)
  | Cstubs_lwt_preemptive (* Lwt preemptive threading *)

(* Ctypes.cstubs errno policy *)
type cstubs_errno =
  | Cstubs_ignore_errno (* Default: errno not accessed *)
  | Cstubs_return_errno (* Functions return (retval, errno) pairs *)

(* Ctypes.cstubs configuration for a library *)
type target_cstubs = {
  cstubs_external_library_name : string; (* Name for generated C library *)
  cstubs_type_description : cstubs_description option; (* Types functor -> instance *)
  cstubs_function_description : cstubs_description option; (* Functions functor -> instance *)
  cstubs_generated_types : string; (* Intermediate types module name *)
  cstubs_generated_entry_point : string; (* Main entry module (e.g., "C") *)
  cstubs_headers : string list; (* C headers to include *)
  cstubs_concurrency : cstubs_concurrency; (* Concurrency policy *)
  cstubs_errno : cstubs_errno; (* Errno handling policy *)
}

type target = {
  target_name : Name.t;
  target_type : Typ.t;
  target_cbits : target_cbits;
  target_obits : target_obits;
  target_cstubs : target_cstubs option;
  target_extras : target_extra list;
  target_buildable : runtime_bool;
  target_installable : runtime_bool;
}

let new_target_cbits =
  {
    target_cdir = current_dir;
    target_csources = [];
    target_cflags = [];
    target_clibs = [];
    target_clibpaths = [];
    target_cpkgs = [];
  }

let new_target_obits =
  {
    target_oflags = [];
    target_builddeps = [];
    target_pp = None;
    target_srcdir = [ current_dir ];
    target_extradeps = [];
    target_stdlib = Stdlib_Standard;
  }

let new_target_cstubs =
  {
    cstubs_external_library_name = "";
    cstubs_type_description = None;
    cstubs_function_description = None;
    cstubs_generated_types = "Types_generated";
    cstubs_generated_entry_point = "C";
    cstubs_headers = [];
    cstubs_concurrency = Cstubs_sequential;
    cstubs_errno = Cstubs_ignore_errno;
  }

let new_target n ty buildable installable =
  Printf.printf "Debug_new_target %s build %b install %b\n" (Name.to_string n) buildable installable;
  {
    target_name = n;
    target_buildable = runtime_def buildable;
    target_installable = runtime_def installable;
    target_type = ty;
    target_extras = [];
    target_cbits = new_target_cbits;
    target_obits = new_target_obits;
    target_cstubs = None;
  }

let new_target_extra objs =
  {
    target_extra_objects = objs;
    target_extra_builddeps = [];
    target_extra_oflags = [];
    target_extra_cflags = [];
    target_extra_pp = None;
  }

let get_target_name target = Name.to_string target.target_name
let get_target_dest_name target = Name.get_dest_name target.target_name
let get_target_clibname target = Name.get_clibname target.target_name
let is_lib target = Typ.is_lib target.target_type

let get_ocaml_compiled_types target =
  let nat, byte =
    if is_lib target then
      (Gconf.get_target_option "library-native", Gconf.get_target_option "library-bytecode")
    else
      (Gconf.get_target_option "executable-native", Gconf.get_target_option "executable-bytecode")
  in
  (if nat then [ Native ] else []) @ if byte then [ ByteCode ] else []

let get_debug_profile target =
  if is_lib target then
    (Gconf.get_target_option "library-debugging", Gconf.get_target_option "library-profiling")
  else
    (Gconf.get_target_option "executable-debugging", Gconf.get_target_option "executable-profiling")

let get_compilation_opts target =
  let debug, prof = get_debug_profile target in
  (Normal :: (if debug then [ WithDebug ] else [])) @ if prof then [ WithProf ] else []

let get_all_builddeps target =
  let targetWideDeps = target.target_obits.target_builddeps in
  let fileSpecificDeps =
    List.map (fun extra -> extra.target_extra_builddeps) target.target_extras
  in
  targetWideDeps @ List.concat fileSpecificDeps

let find_extra_matching target s =
  List.filter (fun extra -> List.mem s extra.target_extra_objects) target.target_extras
