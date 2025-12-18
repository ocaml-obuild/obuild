(** META file parsing for OCamlfind package metadata

    This module provides parsing and querying of OCamlfind META files,
    which describe OCaml library packages, their dependencies, and
    compilation flags. *)

(** {1 Predicates} *)

(** Compilation predicates used in META files to conditionally specify
    library properties based on compilation mode, threading model, etc. *)
module Predicate : sig
  type t =
    | Byte              (** Bytecode compilation *)
    | Native            (** Native code compilation *)
    | Toploop           (** Interactive toplevel *)
    | CreateToploop     (** Creating custom toplevel *)
    | Plugin            (** Dynamically loaded plugin *)
    | Mt                (** Multi-threading *)
    | Mt_vm             (** VM-level threading *)
    | Mt_posix          (** POSIX threading *)
    | Gprof             (** Profiling with gprof *)
    | Autolink          (** Automatic linking *)
    | Syntax            (** Syntax extension *)
    | Preprocessor      (** Preprocessor *)
    | Camlp4o           (** Camlp4 original syntax *)
    | Camlp4r           (** Camlp4 revised syntax *)
    | Ppx_driver        (** PPX driver *)
    | Neg of t          (** Negated predicate *)
    | Unknown of string (** Unknown/custom predicate *)

  val to_string : t -> string
  (** Convert predicate to string representation *)

  val of_string : string -> t
  (** Parse predicate from string. Handles negation with '-' prefix *)
end

(** {1 Exceptions} *)

exception LibraryNotFound of string
(** Raised when a library cannot be found in the OCamlfind path *)

exception SubpackageNotFound of string
(** Raised when a subpackage cannot be found within a package *)

exception ArchiveNotFound of Filepath.filepath * Libname.t * Predicate.t list
(** Raised when no suitable archive is found for given predicates *)

exception MetaParseError of Filepath.filepath * string
(** Raised when META file parsing fails *)

(** {1 Package Metadata} *)

(** Package metadata structure and operations *)
module Pkg : sig
  (** Package record containing all META file fields *)
  type t = {
    name : string;
    (** Package name *)

    requires : (Predicate.t list * Libname.t list) list;
    (** Dependencies with predicates *)

    directory : string;
    (** Package directory (can be relative, absolute, or use special prefixes) *)

    description : string;
    (** Human-readable description *)

    exists_if : string;
    (** Condition for package existence *)

    preprocessor : string;
    (** Preprocessor command *)

    ppx : (Predicate.t list * string) option;
    (** PPX rewriter with predicates *)

    ppxopt : (Predicate.t list * string) list;
    (** PPX options with predicates *)

    browse_interface : string;
    (** Interface browsing information *)

    type_of_threads : string;
    (** Threading type *)

    archives : (Predicate.t list * string) list;
    (** Archive files with predicates *)

    warning : (Predicate.t list * string) list;
    (** Warnings with predicates *)

    append_archives : (Predicate.t list * string) list;
    (** Additional archives to append *)

    version : string;
    (** Package version *)

    assignment : (string * string) list;
    (** Custom variable assignments *)

    linkopts : (Predicate.t list option * string) list;
    (** Linker options with optional predicates *)

    subs : t list;
    (** Subpackages *)
  }

  val make : string -> t
  (** [make name] creates an empty package with the given name *)

  val iter : (t -> unit) -> t -> unit
  (** [iter f pkg] applies [f] to [pkg] and all its subpackages recursively *)

  val find : string list -> t -> t
  (** [find subnames pkg] finds a subpackage by traversing the hierarchy.
      @raise SubpackageNotFound if subpackage not found *)

  val get_syntaxes : t -> (Predicate.t list * string) list
  (** [get_syntaxes pkg] extracts syntax extensions from package archives *)

  val satisfy : Predicate.t list -> Predicate.t list -> bool
  (** [satisfy preds constraints] checks if predicates satisfy constraints *)

  val is_syntax_ : t -> bool
  (** [is_syntax_ pkg] returns true if package defines syntax extensions *)

  val write : Filepath.filepath -> t -> unit
  (** [write path package] writes package to META file at path *)

  (** The following functions operate on the outer module's [t] type
      (filepath * Pkg.t), defined below *)

  type meta_t = Filepath.filepath * t
  (** Alias for the full META type to avoid forward reference *)

  val is_syntax : meta_t -> Libname.t -> bool
  (** [is_syntax meta dep] checks if dependency is a syntax extension *)

  val get_archive_with_filter : meta_t -> Libname.t -> Predicate.t list -> (Predicate.t list * string) list
  (** [get_archive_with_filter meta dep preds] gets archives matching predicates.
      Returns list of (predicates, archive_name) pairs *)

  val get_archive : meta_t -> Libname.t -> Predicate.t list -> string
  (** [get_archive meta dep preds] gets the first matching archive *)
end

(** {1 Main Types} *)

(** A parsed META file with its location and package structure *)
type t = Filepath.filepath * Pkg.t

(** {1 META File Operations} *)

val path_warning : bool ref
(** Enable warnings for path resolution issues *)

val parse : Filepath.filepath -> string -> string -> Pkg.t
(** [parse filepath content pkg_name] parses META file content.
    @param filepath Source file path (for error messages)
    @param content The META file content
    @param pkg_name The package name
    @return Parsed package structure *)

val find_lib_path : string -> Filepath.filepath
(** [find_lib_path name] locates the META file for library [name].
    Searches in OCamlfind paths for either:
    - [<path>/<name>/META]
    - [<path>/META.<name>]
    @raise LibraryNotFound if library not found in any path *)

val find_lib : string -> t
(** [find_lib name] locates and parses the META file for library [name].
    @raise LibraryNotFound if library not found
    @raise MetaParseError if parsing fails *)

val resolve_directory : Filepath.filepath -> Filepath.filepath -> string -> Filepath.filepath
(** [resolve_directory stdlib basePath directory] resolves META directory specifications:
    - [""] or ["."] -> basePath
    - ["^"] -> parent of basePath
    - ["^subdir"] -> parent/subdir
    - ["+stdlib"] -> stdlib/stdlib
    - absolute path -> as-is
    - relative path -> basePath/path *)

val get_include_dir : Filepath.filepath -> t -> Filepath.filepath
(** [get_include_dir stdlib (path, pkg)] resolves the include directory for a package *)

val get_include_dir_with_subpath : Filepath.filepath -> t -> string list -> Filepath.filepath
(** [get_include_dir_with_subpath stdlib (path, pkg) subnames] resolves the include directory
    for a subpackage by traversing the hierarchy.
    @raise SubpackageNotFound if subpackage not found *)
