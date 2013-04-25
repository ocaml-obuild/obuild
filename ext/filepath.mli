(** The module [Filepath] defines two types, [filepath] and [filename]
    to represent paths and file names in a file system.

    * a [filepath] represent a path in a filesystem. It can be
    relative or absolute, and is composed of components. The last
    component can correspond to a directory or a file in a
    filesystem. Other components correspond to directories.

    * a [filename] encapsulate the name of a file.
*)

(** Exceptions *)

(** [EmptyFilename] is raised by [fn] when trying to create a value of
    type [filename] out of strings "", "." or ".." *)
exception EmptyFilename

(** [InvalidFilename fn] is raised by [fn] when trying to create a
    value of type [filename] when [fn] contains [Filename.dir_sep]. *)
exception InvalidFilename of string

(** Types *)

(** Type representing a path in a filesystem. *)
type filepath

(** Type representing  a file in a filesystem. *)
type filename

(** Filename guaranteed to point to no valid file. Useful for
    initializing structures that have a field of type [filename]. *)
val emptyFn : filename

(** Filepath pointing to the current working directory. *)
val currentDir : filepath

(** Functions to convert the above types to and from string. *)

val fp_to_string : filepath -> string
val fn_to_string : filename -> string
val fp : string -> filepath
val fn : string -> filename

(** [got_dirsep s] returns [true] if [s] contains [Filename.dir_sep],
    i.e. "/" on Unix. *)
val got_dirsep : string -> bool

(** [valid_fn s] returns [true] if [s] is a valid file name, i.e. not
    ".", "..", not containing [Filename.dir_sep]. *)
val valid_fn : string -> bool

(** [fp1 <//> fp2] concatenate [fp2] to [fp1]. [fp2] cannot be an absolute
    path. *)
val ( <//> ) : filepath -> filepath -> filepath

(** [fp </> fn] concatenate [fn] to [fp]. *)
val ( </> ) : filepath -> filename -> filepath

(** [fn <.> ext] appends the extension [ext] to [fn]. *)
val ( <.> ) : filename -> string -> filename

(** [with_optpath fp fn] is equivalent to [fp </> fn] if [fp <> None],
    otherwise equivalent to [currentDir </> fn]. *)
val with_optpath : filepath option -> filename -> filepath

(** [in_current_dir fn] is equivalent to [currentDir </> fn]. *)
val in_current_dir : filename -> filepath

(** [path_length fp] returns the number of components in [fp],
    including the last (basename) one. *)
val path_length : filepath -> int

(** Analogous to [Filename.dirname], but operate on [filepath]s. *)
val path_dirname : filepath -> filepath

(** Analogous to [Filename.basename], but operate on [filepath]s. *)
val path_basename : filepath -> filename

(** [path_parent fp] is equivalent to [path_dirname (path_dirname fp)]. *)
val path_parent : filepath -> filepath

(** Analogous to [Filename.chop_extension], but for [filename]s. *)
val chop_extension : filename -> filename
