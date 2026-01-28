(** Project configuration file (.obuild) parsing and representation

    This module handles parsing and representation of obuild project files,
    which define libraries, executables, tests, and build configuration. *)

(** {1 Exceptions} *)

exception NoConfFile
(** Raised when no .obuild configuration file is found *)

exception MultipleConfFiles
(** Raised when multiple .obuild files exist in the same directory *)

exception InvalidConfFile of string
(** Raised when configuration file has invalid syntax or structure *)

exception MissingField of string
(** Raised when a required field is missing *)

exception UnknownDependencyName of string
(** Raised when a dependency name cannot be resolved *)

exception UnsupportedFutureVersion of int
(** Raised when .obuild file uses a newer format version *)

exception ModuleDoesntExist of Target.target * Hier.t
(** Raised when a declared module file doesn't exist *)

exception ModuleListEmpty of Libname.t
(** Raised when a library has no modules defined *)

exception FileDoesntExist of Target.target * Filepath.filename
(** Raised when a referenced file doesn't exist *)

exception LicenseFileDoesntExist of Filepath.filepath
(** Raised when the declared license file doesn't exist *)

exception BlockSectionAsValue of string
(** Raised when trying to use a block section as a simple value *)

exception ExecutableWithNoMain of string
(** Raised when an executable has no main file defined *)

exception UnknownStdlib of string
(** Raised when an unknown stdlib is specified *)

exception UnknownExtraDepFormat of string
(** Raised when extra dependency format is invalid *)

exception UnknownFlag of string
(** Raised when an unknown flag is referenced *)

exception BadOcamlVersion of (string * Expr.t)
(** Raised when OCaml version constraint is invalid *)

exception LibraryNotFound of Libname.t
(** Raised when a library cannot be found in the project *)

exception ExecutableNotFound of string
(** Raised when an executable cannot be found in the project *)

exception BenchNotFound of string
(** Raised when a benchmark cannot be found in the project *)

exception TestNotFound of string
(** Raised when a test cannot be found in the project *)

exception ExampleNotFound of string
(** Raised when an example cannot be found in the project *)

(** {1 Library Configuration} *)

module Library : sig
  type t = {
    name : Libname.t;
    description : string;
    target : Target.target;
    modules : Hier.t list;
    pack : bool;
    syntax : bool;
    subs : t list;
  }
  (** Library configuration with optional sublibraries *)

  val make : Libname.t -> t
  (** Create a new library with default settings *)

  val make_prefix : Libname.t -> string -> t
  (** Create a sublibrary with prefixed name *)

  val make_from_string : string -> t
  (** Create a library from string name *)

  val to_target : t -> Target.target
  (** Extract target from library *)

  val to_targets : t -> Target.target list
  (** Get all targets (library + sublibraries) *)

  val flatten : t -> t list
  (** Flatten library hierarchy into list *)

  val find : t list -> Libname.t -> t
  (** Find library by name in list
      @raise LibraryNotFound if not found *)

  val check_modules_not_empty : t -> unit
  (** Verify library has modules defined
      @raise ModuleListEmpty if empty *)
end

(** {1 Executable Configuration} *)

module Executable : sig
  type t = {
    name : string;
    main : Filepath.filename;
    target : Target.target;
  }
  (** Executable configuration *)

  val make : string -> t
  (** Create new executable with default settings *)

  val to_target : t -> Target.target
  (** Extract target from executable *)

  val find : t list -> string -> t
  (** Find executable by name in list
      @raise ExecutableNotFound if not found *)
end

(** {1 Test Configuration} *)

module Test : sig
  type test_type = ExitCode
  (** Test type - currently only exit code tests supported *)

  type t = {
    name : string;
    main : Filepath.filename;
    target : Target.target;
    rundir : Filepath.filepath option;
    runopt : string list;
    type_ : test_type;
  }
  (** Test configuration *)

  val make :
    name:string ->
    main:Filepath.filename ->
    target:Target.target ->
    rundir:Filepath.filepath option ->
    runopt:string list ->
    t
  (** Create test from parameters. Buildable defaults to CLI option "build-tests". *)

  val to_target : t -> Target.target
  (** Extract target from test *)

  val find : t list -> string -> t
  (** Find test by name in list
      @raise TestNotFound if not found *)
end

(** {1 Benchmark Configuration} *)

module Bench : sig
  type t = {
    name : string;
    main : Filepath.filename;
    target : Target.target;
  }
  (** Benchmark configuration *)

  val make :
    name:string ->
    main:Filepath.filename ->
    target:Target.target ->
    t
  (** Create benchmark from parameters. Buildable defaults to CLI option "build-benchs". *)

  val to_target : t -> Target.target
  (** Extract target from benchmark *)

  val find : t list -> string -> t
  (** Find benchmark by name in list
      @raise BenchNotFound if not found *)
end

(** {1 Example Configuration} *)

module Example : sig
  type t = {
    name : string;
    main : Filepath.filename;
    target : Target.target;
  }
  (** Example executable configuration *)

  val make :
    name:string ->
    main:Filepath.filename ->
    target:Target.target ->
    t
  (** Create example from parameters. Buildable defaults to CLI option "build-examples". *)

  val to_target : t -> Target.target
  (** Extract target from example *)

  val find : t list -> string -> t
  (** Find example by name in list
      @raise ExampleNotFound if not found *)
end

(** {1 Flag Configuration} *)

module Flag : sig
  type t = {
    name : string;
    description : string;
    default : bool option;
  }
  (** Compile-time flag configuration *)
end

(** {1 Generator Configuration} *)

module Generator : sig
  (** How to match source files for this generator *)
  type match_type =
    | Match_suffix of string      (** Match by file extension (e.g., "mly") *)
    | Match_filename of string    (** Match by exact filename (e.g., "VERSION") *)
    | Match_pattern of string     (** Match by glob pattern (e.g., "*.txt") *)
    | Match_directory             (** Match directories *)

  type t = {
    name : string;                    (** Generator name for reference *)
    match_type : match_type;          (** How to match source files *)
    command : string;                 (** Command template with variables *)
    outputs : string list;            (** Output file patterns *)
    module_name : string option;      (** Module name pattern if different from base *)
    multi_input : bool;               (** Whether this generator can take multiple inputs *)
  }
  (** Custom generator configuration *)

  val make : string -> t
  (** Create a new generator with default settings *)
end

(** {1 Main Project Type} *)

type t = {
  name : string;
  version : string;
  synopsis : string;
  description : string;
  license : string;
  license_file : Filepath.filepath option;
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
  extra_srcs : Filepath.filepath list;
  extra_tools : Filepath.filename list;
  configure_script : Filepath.filepath option;
  ocaml_extra_args : string list option;
}
(** Project configuration structure *)

val make : t
(** Empty project with default values *)

(** {1 Project File Operations} *)

val findPath : unit -> Filepath.filepath
(** Find .obuild configuration file in current directory.
    @raise NoConfFile if no .obuild file found
    @raise MultipleConfFiles if multiple .obuild files found *)

val digest : unit -> Digest.t
(** Compute digest of project configuration file *)

val write : Filepath.filepath -> t -> unit
(** [write file proj] writes project configuration to file *)

val check : t -> unit
(** [check proj] validates the project configuration.
    Checks that required files and modules exist on disk.
    @raise ModuleDoesntExist if a declared module doesn't exist
    @raise FileDoesntExist if a referenced file doesn't exist
    @raise LicenseFileDoesntExist if license file is missing
    @raise ExecutableWithNoMain if executable has no main file
    @raise BadOcamlVersion if OCaml version constraint fails *)

(** {1 Target Operations} *)

val get_all_buildable_targets : t -> (string * bool) list -> Target.target list
(** [get_all_buildable_targets proj user_flags] returns all buildable targets
    given user-specified flags *)

val get_all_installable_targets : t -> (string * bool) list -> Target.target list
(** [get_all_installable_targets proj user_flags] returns all installable targets
    given user-specified flags *)

(** {1 Lookup Functions} *)

val find_lib : t -> Libname.t -> Library.t
(** Find library in project by name
    @raise LibraryNotFound if not found *)

val find_exe : t -> string -> Executable.t
(** Find executable in project by name
    @raise ExecutableNotFound if not found *)

val find_test : t -> string -> Test.t
(** Find test in project by name
    @raise TestNotFound if not found *)

val find_bench : t -> string -> Bench.t
(** Find benchmark in project by name
    @raise BenchNotFound if not found *)

val find_example : t -> string -> Example.t
(** Find example in project by name
    @raise ExampleNotFound if not found *)
