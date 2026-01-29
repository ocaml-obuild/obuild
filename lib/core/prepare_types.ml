(** Shared type definitions for the prepare module and its sub-modules

    This module contains type definitions used across the preparation phase, including module
    descriptions, compilation state, and build steps. *)

open Filepath
open Analyze
open Target

(** Thread usage flag for modules *)
type use_thread_flag =
  | NoThread
  | WithThread

(** Thread implementation type *)
type thread_type =
  | VMThread
  | PosixThread
  | DefaultThread
  | NoThreads

(** OCaml file classification *)
type ocaml_file_type =
  | GeneratedModule
  | SimpleModule

type build_state = { bstate_config : project_config }
(** Build state persists for the entire build process *)

type dir_spec = {
  src_dir : filepath;
  dst_dir : filepath;
  include_dirs : filepath list;
}
(** Directory specification for compilation *)

(** Compilation step in the build DAG *)
type compile_step =
  | CompileModule of Hier.t
  | CompileInterface of Hier.t
  | CompileDirectory of Hier.t
  | CompileC of filename
  | GenerateCstubsTypes of Libname.t (* Generate types_generated.ml *)
  | GenerateCstubsFunctions of Libname.t (* Generate C.ml and stubs.c *)
  | CompileCstubsC of Libname.t (* Compile generated C stubs *)
  | RunGenerateBlock of Target.target_generate (* Run explicit generate block *)
  | LinkTarget of Target.target
  | CheckTarget of Target.target

(** Module descriptor system *)
module Module = struct
  exception DependsItself of Hier.t
  exception DependenciesProblem of Hier.t list
  exception DependencyNoOutput
  exception NotFound of (filepath list * Hier.t)

  module Intf = struct
    type t = {
      mtime : float;
      path : filepath;
    }

    let make mtime path = { mtime; path }
  end

  module File = struct
    type t = {
      use_threads : use_thread_flag;
      path : filepath;
      mtime : float;
      type_ : ocaml_file_type;
      intf_desc : Intf.t option;
      use_pp : Pp.t;
      oflags : string list;
      dep_cwd_modules : Hier.t list;
      dep_other_modules : Modname.t list;
    }

    let make use_threads path mtime type_ intf_desc use_pp oflags dep_cwd_modules dep_other_modules
        =
      {
        use_threads;
        path;
        mtime;
        type_;
        intf_desc;
        use_pp;
        oflags;
        dep_cwd_modules;
        dep_other_modules;
      }
  end

  module Dir = struct
    type t = {
      path : filepath;
      modules : Hier.t list;
    }

    let make path modules = { path; modules }
  end

  type t =
    | DescFile of File.t
    | DescDir of Dir.t

  let file_has_interface mdescfile = Fugue.maybe false (fun _ -> true) mdescfile.File.intf_desc

  let has_interface = function
    | DescFile dfile -> file_has_interface dfile
    | DescDir _ -> false

  let make_dir path modules = DescDir (Dir.make path modules)

  let make_file use_threads path mtime type_ intf_desc use_pp oflags dep_cwd_modules
      dep_other_modules =
    DescFile
      (File.make use_threads path mtime type_ intf_desc use_pp oflags dep_cwd_modules
         dep_other_modules)
end

type compilation_state = {
  compilation_modules : (Hier.t, Module.t) Hashtbl.t;
  compilation_csources : filename list;
  compilation_dag : compile_step Dag.t;
  compilation_pp : Pp.t;
  compilation_filesdag : Filetype.id Dag.t;
  compilation_builddir_c : filepath;
  compilation_builddir_ml : Types.ocaml_compilation_option -> filepath;
  compilation_include_paths : Types.ocaml_compilation_option -> Hier.t -> filepath list;
  compilation_linking_paths : filepath list;
  compilation_linking_paths_d : filepath list;
  compilation_linking_paths_p : filepath list;
  compilation_c_include_paths : filepath list;
  compilation_c_linking_paths : filepath list;
}
(** Compilation state - represents a single compilation target *)

(** Convert compile step to string for debugging *)
let string_of_compile_step cs =
  match cs with
  | CompileDirectory x -> "dir " ^ Hier.to_string x
  | CompileModule x -> "mod " ^ Hier.to_string x
  | CompileInterface x -> "intf " ^ Hier.to_string x
  | CompileC x -> "C " ^ Filepath.fn_to_string x
  | GenerateCstubsTypes x -> "cstubs-types " ^ Libname.to_string x
  | GenerateCstubsFunctions x -> "cstubs-funcs " ^ Libname.to_string x
  | CompileCstubsC x -> "cstubs-c " ^ Libname.to_string x
  | RunGenerateBlock x -> "generate " ^ Hier.to_string x.Target.generate_module
  | LinkTarget x -> "link " ^ Target.get_target_name x
  | CheckTarget x -> "check " ^ Target.get_target_name x
