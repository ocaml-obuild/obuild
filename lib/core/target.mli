(** Build target definitions and utilities *)

(** Exception raised when a target name lacks a type prefix *)
exception TargetNameNoType of string

(** Exception raised when a target has an unknown type prefix *)
exception TargetUnknownType of string * string

(** Exception raised when a target name cannot be recognized *)
exception TargetNotRecognized of string

(** Target types *)
module Typ : sig
  type t = Lib | Exe | Test | Bench

  val is_lib : t -> bool
  (** Check if target type is a library *)
end

(** Target name representation with type prefixes *)
module Name : sig
  type t =
    | Lib of Libname.t
    | Exe of string
    | Test of string
    | Bench of string
    | Example of string

  val to_string : t -> string
  (** Convert target name to string with type prefix (e.g., "exe-foo", "lib-bar") *)

  val of_string : string -> t
  (** Parse target name from string
      @raise TargetNameNoType if name has no type prefix
      @raise TargetUnknownType if type prefix is not recognized
      @raise TargetNotRecognized if name format is invalid *)

  val to_dirname : t -> Filepath.filename
  (** Convert target name to directory name for build output *)

  val get_clibname : t -> string
  (** Get C library stub name for this target *)

  val get_dest_name : t -> string
  (** Get destination file name (without extension) for this target *)
end

(** Standard library usage mode *)
type target_stdlib = Stdlib_None | Stdlib_Standard | Stdlib_Core

(** Runtime-evaluated boolean (constant or variable reference) *)
type runtime_bool =
  | BoolConst of bool
  | BoolVariable of string

val runtime_def : bool -> runtime_bool
(** Create runtime boolean from constant value *)

(** C compilation bits for a target *)
type target_cbits = {
  target_cdir      : Filepath.filepath;
  target_csources  : Filepath.filename list;
  target_cflags    : string list;
  target_clibs     : string list;
  target_clibpaths : Filepath.filepath list;
  target_cpkgs     : Dependencies.cdependency list;
}

(** OCaml compilation bits for a target *)
type target_obits = {
  target_srcdir    : Filepath.filepath list;
  target_builddeps : Dependencies.dependency list;
  target_oflags    : string list;
  target_pp        : Pp.Type.t option;
  target_extradeps : (Hier.t * Hier.t) list;
  target_stdlib    : target_stdlib;
}

(** Extra per-file compilation settings *)
type target_extra = {
  target_extra_objects   : string list;
  target_extra_builddeps : Dependencies.dependency list;
  target_extra_oflags    : string list;
  target_extra_cflags    : string list;
  target_extra_pp        : Pp.Type.t option;
}

(** Ctypes.cstubs description: pair of functor module -> generated instance name *)
type cstubs_description = {
  cstubs_functor  : Hier.t;   (** User's functor module, e.g., Bindings.Types *)
  cstubs_instance : string;   (** Generated instance name, e.g., Types_gen *)
}

(** Ctypes.cstubs concurrency policy *)
type cstubs_concurrency =
  | Cstubs_sequential      (** Default: no special concurrency support *)
  | Cstubs_unlocked        (** Release runtime lock during C calls *)
  | Cstubs_lwt_jobs        (** Lwt jobs-based concurrency *)
  | Cstubs_lwt_preemptive  (** Lwt preemptive threading *)

(** Ctypes.cstubs errno policy *)
type cstubs_errno =
  | Cstubs_ignore_errno    (** Default: errno not accessed *)
  | Cstubs_return_errno    (** Functions return (retval, errno) pairs *)

(** Ctypes.cstubs configuration for a library *)
type target_cstubs = {
  cstubs_external_library_name : string;              (** Name for generated C library *)
  cstubs_type_description      : cstubs_description option;  (** Types functor -> instance *)
  cstubs_function_description  : cstubs_description option;  (** Functions functor -> instance *)
  cstubs_generated_types       : string;              (** Intermediate types module name *)
  cstubs_generated_entry_point : string;              (** Main entry module (e.g., "C") *)
  cstubs_headers               : string list;         (** C headers to include *)
  cstubs_concurrency           : cstubs_concurrency;  (** Concurrency policy *)
  cstubs_errno                 : cstubs_errno;        (** Errno handling policy *)
}

(** Complete build target definition *)
type target = {
  target_name        : Name.t;
  target_type        : Typ.t;
  target_cbits       : target_cbits;
  target_obits       : target_obits;
  target_cstubs      : target_cstubs option;
  target_extras      : target_extra list;
  target_buildable   : runtime_bool;
  target_installable : runtime_bool;
}

val new_target_cbits : target_cbits
(** Empty C compilation bits with default values *)

val new_target_obits : target_obits
(** Empty OCaml compilation bits with default values *)

val new_target_cstubs : target_cstubs
(** Default cstubs configuration with empty values *)

val new_target : Name.t -> Typ.t -> bool -> bool -> target
(** [new_target name typ buildable installable] creates a new target

    @param name target name
    @param typ target type
    @param buildable whether target can be built
    @param installable whether target can be installed *)

val new_target_extra : string list -> target_extra
(** Create extra compilation settings for specific object files *)

val get_target_name : target -> string
(** Get target name as string with type prefix *)

val get_target_dest_name : target -> string
(** Get destination file name (without extension) *)

val get_target_clibname : target -> string
(** Get C library stub name *)

val is_lib : target -> bool
(** Check if target is a library *)

val get_ocaml_compiled_types : target -> Types.ocaml_compiled_type list
(** Get OCaml compilation types (Native/ByteCode) based on target type and global config *)

val get_debug_profile : target -> bool * bool
(** Get (debugging, profiling) flags based on target type and global config *)

val get_compilation_opts : target -> Types.ocaml_compilation_option list
(** Get compilation options (Normal/WithDebug/WithProf) based on target config *)

val get_all_builddeps : target -> Dependencies.dependency list
(** Get all build dependencies including target-wide and file-specific deps *)

val find_extra_matching : target -> string -> target_extra list
(** Find extra compilation settings for a specific object file *)
