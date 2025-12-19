(** The module [Filepath] defines two types, [filepath] and [filename] to represent paths and file
    names in a file system.

    * a [filepath] represent a path in a filesystem. It can be relative or absolute, and is composed
    of components. The last component can correspond to a directory or a file in a filesystem. Other
    components correspond to directories.

    * a [filename] encapsulate the name of a file. *)

(** Exceptions *)

exception EmptyFilename
(** [EmptyFilename] is raised by [fn] when trying to create a value of type [filename] out of
    strings "", "." or ".." *)

exception InvalidFilename of string
(** [InvalidFilename fn] is raised by [fn] when trying to create a value of type [filename] when
    [fn] contains [Filename.dir_sep]. *)

(** Types *)

type filepath
(** Type representing a path in a filesystem. *)

type filename
(** Type representing a file in a filesystem. *)

val empty_fn : filename
(** Filename guaranteed to point to no valid file. Useful for initializing structures that have a
    field of type [filename]. *)

val current_dir : filepath
(** Filepath pointing to the current working directory. *)

(** Functions to convert the above types to and from string. *)

val fp_to_string : filepath -> string
val fn_to_string : filename -> string
val fp : string -> filepath
val fn : string -> filename
val is_absolute : filepath -> bool

val valid_fn : string -> bool
(** [valid_fn s] returns [true] if [s] is a valid file name, i.e. not ".", "..", not containing
    [Filename.dir_sep]. *)

val ( <//> ) : filepath -> filepath -> filepath
(** [fp1 <//> fp2] concatenate [fp2] to [fp1]. [fp2] cannot be an absolute path. *)

val ( </> ) : filepath -> filename -> filepath
(** [fp </> fn] concatenate [fn] to [fp]. *)

val ( <.> ) : filename -> string -> filename
(** [fn <.> ext] appends the extension [ext] to [fn]. *)

val in_current_dir : filename -> filepath
(** [in_current_dir fn] is equivalent to [current_dir </> fn]. *)

val path_length : filepath -> int
(** [path_length fp] returns the number of components in [fp], including the last (basename) one. *)

val path_dirname : filepath -> filepath
(** Analogous to [Filename.dirname], but operate on [filepath]s. *)

val path_basename : filepath -> filename
(** Analogous to [Filename.basename], but operate on [filepath]s. *)

val chop_extension : filename -> filename
(** Analogous to [Filename.chop_extension], but for [filename]s. *)
