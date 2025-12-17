(** Module hierarchy management

    This module handles hierarchical module names (like A.B.C) and their
    mapping to filesystem paths. It manages the lookup and caching of module
    files across directory structures. *)

(** The type of a module hierarchy - a list of module names *)
type t = Modname.t list

(** File entry types representing different kinds of source files *)
type file_entry =
  | FileEntry of (Filepath.filepath * Filepath.filepath)
    (** Normal source file: (root_path, full_path) *)
  | GeneratedFileEntry of (Filepath.filepath * Filepath.filepath * Filepath.filename)
    (** Generated source file: (root_path, full_path, generated_filename) *)
  | DirectoryEntry of (Filepath.filepath * Filepath.filepath)
    (** Directory representing a module: (root_path, full_path) *)

(** {1 Exceptions} *)

exception EmptyModuleHierarchy
(** Raised when attempting to create an empty module hierarchy *)

(** {1 Construction and Conversion} *)

val make : Modname.t list -> t
(** [make mods] creates a module hierarchy from a list of module names.
    @raise EmptyModuleHierarchy if the list is empty *)

val of_string : string -> t
(** [of_string "A.B.C"] creates a module hierarchy by splitting on dots *)

val of_modname : Modname.t -> t
(** [of_modname m] creates a single-level hierarchy from module name [m] *)

val of_filename : Filepath.filename -> t
(** [of_filename fn] creates a hierarchy from a filename,
    removing extension and capitalizing *)

val to_string : t -> string
(** [to_string hier] converts to dot-separated string (e.g., "A.B.C") *)

val to_node : t -> Modname.t list
(** [to_node hier] returns the underlying module name list *)

(** {1 Hierarchy Navigation} *)

val root : t -> Modname.t
(** [root hier] returns the first module in the hierarchy *)

val leaf : t -> Modname.t
(** [leaf hier] returns the last module in the hierarchy *)

val parent : t -> t option
(** [parent hier] returns the parent hierarchy, or [None] if single-level.
    Example: parent [A; B; C] = Some [A; B] *)

val lvl : t -> int
(** [lvl hier] returns the depth level (0-indexed from root).
    Example: lvl [A; B; C] = 2 *)

val append : t -> Modname.t -> t
(** [append hier m] appends module [m] to hierarchy *)

(** {1 Path Conversion} *)

val to_dirpath : t -> Filepath.filepath
(** [to_dirpath hier] converts hierarchy to directory path.
    Example: [A; B; C] -> "a/b" (excludes leaf) *)

val add_prefix : Filepath.filepath -> t -> Filepath.filepath
(** [add_prefix prefix hier] combines prefix path with hierarchy path,
    intelligently handling overlapping components *)

val ml_to_ext : Filepath.filepath -> Filetype.t -> Filepath.filepath
(** [ml_to_ext path ext] changes file extension of path to [ext] *)

(** {1 File Lookup} *)

val get_filepath : Filepath.filepath -> t -> Filetype.t -> file_entry option
(** [get_filepath root hier ext] searches for a file matching the hierarchy
    with the given extension. Returns cached result if available. *)

val to_filename : t -> Filepath.filepath -> file_entry option
(** [to_filename hier root] finds the .ml file for hierarchy *)

val to_interface : t -> Filepath.filepath -> file_entry option
(** [to_interface hier root] finds the .mli file for hierarchy *)

val to_directory : t -> Filepath.filepath -> file_entry option
(** [to_directory hier root] finds the directory for hierarchy *)

val to_generators : t -> Filepath.filepath -> file_entry option
(** [to_generators hier root] finds generated source files (e.g., .mll, .mly, .atd) *)

val get_file_entry : t -> Filepath.filepath list -> file_entry
(** [get_file_entry hier paths] searches for hierarchy across multiple root paths,
    trying all lookup methods (filename, directory, generators, interface).
    @raise Not_found if hierarchy not found in any path *)

val get_file_entry_maybe : t -> file_entry option
(** [get_file_entry_maybe hier] returns cached file entry if available *)

(** {1 File Entry Operations} *)

val file_entry_to_string : file_entry -> string
(** [file_entry_to_string entry] converts file entry to debug string *)

val get_src_file : Filepath.filepath -> file_entry -> Filepath.filepath
(** [get_src_file dst_dir entry] returns the source file path from an entry *)

val get_dest_file : Filepath.filepath -> Filetype.t -> t -> Filepath.filepath
(** [get_dest_file dst_dir ext hier] computes destination file path
    for hierarchy with given extension in destination directory.
    @raise Not_found if hierarchy not cached *)

val get_dest_file_ext : Filepath.filepath -> t -> (Filetype.t -> Filetype.t) -> Filepath.filepath
(** [get_dest_file_ext dst_dir hier ext_fn] computes destination file path
    using [ext_fn] to transform the source file type.
    @raise Not_found if hierarchy not cached *)
