open Ext.Filepath
open Ext.Fugue
open Types
open Gconf
open Dependencies

type target_type = Lib | Exe | Test | Bench
type target_stdlib = Stdlib_None | Stdlib_Standard | Stdlib_Core

type runtime_bool = BoolConst of bool
                  | BoolVariable of string

let runtime_def v = BoolConst v

type target_cbits =
    { target_cdir      : filepath
    ; target_csources  : filename list
    ; target_cflags    : string list (* CFLAGS *)
    ; target_clibs     : string list
    ; target_clibpaths : filepath list
    ; target_cpkgs     : cdependency list (* pkg-config name *)
    }

type target_obits =
    { target_srcdir    : filepath
    ; target_builddeps : dependency list
    ; target_oflags    : string list
    ; target_pp        : Pp.pp_type option
    ; target_extradeps : (Hier.t * Hier.t) list
    ; target_stdlib    : target_stdlib
    }

type target_extra =
    { target_extra_objects   : string list     (* targets of those extra settings *)
    ; target_extra_builddeps : dependency list
    ; target_extra_oflags    : string list
    ; target_extra_cflags    : string list
    }

type target =
    { target_name        : name
    ; target_type        : target_type
    ; target_cbits       : target_cbits
    ; target_obits       : target_obits
    ; target_extras      : target_extra list
    ; target_buildable   : runtime_bool
    ; target_installable : runtime_bool
    }

let newTargetCbits =
    { target_cdir      = currentDir
    ; target_csources  = []
    ; target_cflags    = []
    ; target_clibs     = []
    ; target_clibpaths = []
    ; target_cpkgs     = []
    }

let newTargetObits =
    { target_oflags    = []
    ; target_builddeps = []
    ; target_pp        = None
    ; target_srcdir    = currentDir
    ; target_extradeps = []
    ; target_stdlib    = Stdlib_Standard
    }

let newTarget n ty buildable installable =
    { target_name        = n
    ; target_buildable   = runtime_def buildable
    ; target_installable = runtime_def installable
    ; target_type        = ty
    ; target_extras      = []
    ; target_cbits       = newTargetCbits
    ; target_obits       = newTargetObits
    }

let newTargetExtra objs =
    { target_extra_objects   = objs
    ; target_extra_builddeps = []
    ; target_extra_oflags    = []
    ; target_extra_cflags    = []
    }

let get_target_name target = name_to_string target.target_name

let get_target_clibname target =
    match target.target_name with
    | ExeName e     -> "stubs_" ^ e
    | BenchName e   -> "stubs_" ^ e
    | TestName  e   -> "stubs_" ^ e
    | ExampleName e -> "stubs_" ^ e
    | LibName l     -> "stubs_" ^ list_last (lib_name_to_string_nodes l)

(* get the core name of the final object representing the object
 * for an executable/test/bench it will be the name of the executable apart from the extension
 * for a test it will be the name of the library created (.cmxa/.cma) apart from the extension
 *)
let get_target_dest_name target =
    match target.target_name with
    | ExeName e   -> e
    | BenchName e -> "bench-" ^ e
    | TestName e  -> "test-" ^ e
    | ExampleName e  -> "example-" ^ e
    | LibName l   -> String.concat "_" (lib_name_to_string_nodes l)

let is_target_lib target = target.target_type = Lib

let get_ocaml_compiled_types target =
    let (nat,byte) =
        if is_target_lib target
            then (Gconf.get_target_option "library-native", Gconf.get_target_option "library-bytecode")
            else (Gconf.get_target_option "executable-native", Gconf.get_target_option "executable-bytecode")
        in
    (if nat then [Native] else []) @ (if byte then [ByteCode] else [])

let get_debug_profile target =
    if is_target_lib target
        then (Gconf.get_target_option "library-debugging", Gconf.get_target_option "library-profiling")
        else (Gconf.get_target_option "executable-debugging", Gconf.get_target_option "executable-profiling")

let get_compilation_opts target =
    let (debug, prof) = get_debug_profile target in
    Normal :: (if debug then [WithDebug] else []) @ (if prof then [WithProf] else [])

let get_all_builddeps target =
    let targetWideDeps = target.target_obits.target_builddeps in
    let fileSpecificDeps = List.map (fun extra -> extra.target_extra_builddeps) target.target_extras in
    targetWideDeps @ List.concat fileSpecificDeps

let find_extra_matching target s =
    List.filter (fun extra -> List.mem s extra.target_extra_objects) target.target_extras
