(** Shared type definitions for the prepare module and its sub-modules

    This module contains type definitions used across the preparation phase,
    including module descriptions, compilation state, and build steps.
 *)

open Filepath
open Types
open Analyze
open Target

(** Thread usage flag for modules *)
type use_thread_flag = NoThread | WithThread

(** Thread implementation type *)
type thread_type = VMThread | PosixThread | DefaultThread | NoThreads

(** OCaml file classification *)
type ocaml_file_type = GeneratedModule | SimpleModule

(** Build state persists for the entire build process *)
type build_state = { bstate_config : project_config }

(** Directory specification for compilation *)
type dir_spec = {
  src_dir      : filepath;
  dst_dir      : filepath;
  include_dirs : filepath list;
}

(** Compilation step in the build DAG *)
type compile_step =
  | CompileModule    of Hier.t
  | CompileInterface of Hier.t
  | CompileDirectory of Hier.t
  | CompileC         of filename
  | GenerateCstubsTypes     of Libname.t  (** Generate types_generated.ml *)
  | GenerateCstubsFunctions of Libname.t  (** Generate C.ml and stubs.c *)
  | CompileCstubsC          of Libname.t  (** Compile generated C stubs *)
  | LinkTarget       of Target.target
  | CheckTarget      of Target.target

(** Module descriptor system *)
module Module : sig
  exception DependsItself of Hier.t
  exception DependenciesProblem of Hier.t list
  exception DependencyNoOutput
  exception NotFound of (filepath list * Hier.t)

  module Intf : sig
    type t = {
      mtime : float;
      path : filepath
    }
    val make : float -> filepath -> t
  end

  module File : sig
    type t = {
      use_threads  : use_thread_flag;
      path    : filepath;
      mtime   : float;
      type_   : ocaml_file_type;
      intf_desc   : Intf.t option;
      use_pp      : Pp.t;
      oflags      : string list;
      dep_cwd_modules    : Hier.t list;
      dep_other_modules  : Modname.t list;
    }
    val make : use_thread_flag -> filepath -> float -> ocaml_file_type ->
               Intf.t option -> Pp.t -> string list -> Hier.t list ->
               Modname.t list -> t
  end

  module Dir : sig
    type t = {
      path    : filepath;
      modules : Hier.t list
    }
    val make : filepath -> Hier.t list -> t
  end

  type t = DescFile of File.t | DescDir of Dir.t

  val file_has_interface : File.t -> bool
  val has_interface : t -> bool
  val make_dir : filepath -> Hier.t list -> t
  val make_file : use_thread_flag -> filepath -> float -> ocaml_file_type ->
                  Intf.t option -> Pp.t -> string list -> Hier.t list ->
                  Modname.t list -> t
end

(** Compilation state - represents a single compilation target *)
type compilation_state = {
  compilation_modules  : (Hier.t, Module.t) Hashtbl.t;
  compilation_csources : filename list;
  compilation_dag      : compile_step Dag.t;
  compilation_pp       : Pp.t;
  compilation_filesdag : Filetype.id Dag.t;
  compilation_builddir_c  : filepath;
  compilation_builddir_ml : Types.ocaml_compilation_option -> filepath;
  compilation_include_paths : Types.ocaml_compilation_option -> Hier.t -> filepath list;
  compilation_linking_paths : filepath list;
  compilation_linking_paths_d : filepath list;
  compilation_linking_paths_p : filepath list;
  compilation_c_include_paths : filepath list;
  compilation_c_linking_paths : filepath list;
}

(** Convert compile step to string for debugging *)
val string_of_compile_step : compile_step -> string
