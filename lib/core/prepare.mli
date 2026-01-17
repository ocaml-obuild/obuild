(** Build preparation - dependency gathering and compilation state *)

(** Thread usage flag *)
type use_thread_flag = NoThread | WithThread

(** Thread implementation type *)
type thread_type = VMThread | PosixThread | DefaultThread | NoThreads

(** OCaml file type classification *)
type ocaml_file_type =
  | GeneratedModule  (** Auto-generated module *)
  | SimpleModule     (** Regular source module *)

(** Module description and dependency information *)
module Module : sig
  (** Exception raised when a module depends on itself *)
  exception DependsItself of Hier.t

  (** Exception raised when there are dependency problems *)
  exception DependenciesProblem of Hier.t list

  (** Exception raised when a dependency has no output *)
  exception DependencyNoOutput

  (** Exception raised when a module is not found *)
  exception NotFound of (Filepath.filepath list * Hier.t)

  (** Interface file descriptor *)
  module Intf : sig
    type t = {
      mtime : float;
      path : Filepath.filepath
    }
    val make : float -> Filepath.filepath -> t
  end

  (** Source file descriptor *)
  module File : sig
    type t = {
      use_threads  : use_thread_flag;
      path    : Filepath.filepath;
      mtime   : float;
      type_   : ocaml_file_type;
      intf_desc   : Intf.t option;
      use_pp      : Pp.t;
      oflags      : string list;
      dep_cwd_modules    : Hier.t list;
      dep_other_modules  : Modname.t list;
    }
    val make : use_thread_flag -> Filepath.filepath -> float -> ocaml_file_type ->
               Intf.t option -> Pp.t -> string list -> Hier.t list -> Modname.t list -> t
  end

  (** Directory descriptor *)
  module Dir : sig
    type t = {
      path    : Filepath.filepath;
      modules : Hier.t list
    }
    val make : Filepath.filepath -> Hier.t list -> t
  end

  (** Module descriptor - either file or directory *)
  type t = DescFile of File.t | DescDir of Dir.t

  val file_has_interface : File.t -> bool
  (** Check if file module has an interface file *)

  val has_interface : t -> bool
  (** Check if module has an interface file *)

  val make_dir : Filepath.filepath -> Hier.t list -> t
  (** Create directory module descriptor *)

  val make_file : use_thread_flag -> Filepath.filepath -> float -> ocaml_file_type ->
                  Intf.t option -> Pp.t -> string list -> Hier.t list -> Modname.t list -> t
  (** Create file module descriptor *)
end

(** Global build state persisting across all compilations *)
type build_state = { bstate_config : Analyze.project_config }

(** Directory specification for compilation *)
type dir_spec = {
  src_dir      : Filepath.filepath;
  dst_dir      : Filepath.filepath;
  include_dirs : Filepath.filepath list;
}

(** Compilation step types *)
type compile_step =
  | CompileModule    of Hier.t
  | CompileInterface of Hier.t
  | CompileDirectory of Hier.t
  | CompileC         of Filepath.filename
  | GenerateCstubsTypes     of Libname.t  (** Generate types_generated.ml *)
  | GenerateCstubsFunctions of Libname.t  (** Generate C.ml and stubs.c *)
  | CompileCstubsC          of Libname.t  (** Compile generated C stubs *)
  | LinkTarget       of Target.target
  | CheckTarget      of Target.target

val string_of_compile_step : compile_step -> string
(** Convert compilation step to human-readable string *)

(** Compilation state for a single target *)
type compilation_state = {
  compilation_modules  : (Hier.t, Module.t) Hashtbl.t;
  compilation_csources : Filepath.filename list;
  compilation_dag      : compile_step Dag.t;
  compilation_pp       : Pp.t;
  compilation_filesdag : Filetype.id Dag.t;
  compilation_builddir_c  : Filepath.filepath;
  compilation_builddir_ml : Types.ocaml_compilation_option -> Filepath.filepath;
  compilation_include_paths : Types.ocaml_compilation_option -> Hier.t -> Filepath.filepath list;
  compilation_linking_paths : Filepath.filepath list;
  compilation_linking_paths_d : Filepath.filepath list;
  compilation_linking_paths_p : Filepath.filepath list;
  compilation_c_include_paths : Filepath.filepath list;
  compilation_c_linking_paths : Filepath.filepath list;
}

val init : Analyze.project_config -> build_state
(** [init project] initializes build state from analyzed project configuration *)

val prepare_target : build_state -> Filepath.filepath -> Target.target -> Hier.t list -> compilation_state
(** [prepare_target bstate build_dir target modules] prepares compilation for a target

    Creates compilation state including:
    - Module dependency DAG
    - File dependency DAG
    - Include and linking paths
    - C compilation paths

    @param bstate global build state
    @param build_dir target build directory
    @param target the target to prepare
    @param modules top-level modules for this target
    @return compilation state for this target *)

val get_compilation_order : compilation_state -> Hier.t list
(** [get_compilation_order cstate] returns modules in compilation order

    Returns top-level modules in dependency order *)
