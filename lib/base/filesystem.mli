(** The module [Filesystem] contain helpers to browse and operate on
    files and directories of a file system. It uses the abstraction
    provided by the module [Filepath].
*)

(** Exceptions *)

(** Raised by [remove_dir_content] whenever trying to delete a block or
    char device. *)
exception UnexpectedFileType of string

(** Raised by [write_no_partial]. *)
exception WriteFailed

(** Removes the contents of a directory. Raises [UnexpectedFileType]
    if the directory contain a file representing a block or a
    character device. *)
val remove_dir_content : Filepath.filepath -> unit

(** Remove a directory and its content. *)
val remove_dir : Filepath.filepath -> unit

(** [iterate f fp] calls [f] on each filename contained in [fp]
    (excluding "." and ".."). Note that a filename can represent
    either a file or a directory in the file system. *)
val iterate : (Filepath.filename -> unit) -> Filepath.filepath -> unit

(** [list_dir_pred_map f fp] applies [f] to each filename contained in
    [fp] using [iterate], and returns all elements that have been
    obtained when [f] did not return [None]. *)
val list_dir_pred_map :
  (Filepath.filename -> 'a option) -> Filepath.filepath -> 'a list

(** [list_dir_pred pred fp] returns a list of filenames (obtained with
    [iterate] that satisfy the predicate [pred]. *)
val list_dir_pred :
  (Filepath.filename -> bool) -> Filepath.filepath -> Filepath.filename list

(** Returns the modification time of a filepath, or returns [0.] if
    any error occured. *)
val get_modification_time : Filepath.filepath -> float

(** Analogous of [Sys.file_exists] but for a filepath *)
val exists : Filepath.filepath -> bool

(** Analogous of [Sys.is_directory] but for a filepath *)
val is_dir : Filepath.filepath -> bool

(** [mkdir_safe fp perms] creates a directory at [fp] unless a
    directory or a file already exists here. Return [false] if a
    directory already exists, [true] if the directory has just been
    created, and raise an exception [Failure] if a file already exists
    at this location. *)
val mkdir_safe : Filepath.filepath -> Unix.file_perm -> bool

(** Analogous to [ignore (mkdir_safe fp perms). *)
val mkdir_safe_ : Filepath.filepath -> Unix.file_perm -> unit

(** Recursively create directories with [mkdirSafe_] until the all
    directories on the filepath specified as argument exists. *)
val mkdir_safe_recursive : Filepath.filepath -> Unix.file_perm -> unit

(** [write_no_partial fd buf start len] writes [len] chars of [buf]
    starting at [start] in [fd], or raises [WriteFailed] if
    impossible. *)
val write_no_partial : Unix.file_descr -> string -> int -> int -> unit

(** [with_file fp flags perms f] opens the file at [fp] and apply [f]
    to the obtained file descriptor.  *)
val with_file :
  Filepath.filepath ->
  Unix.open_flag list -> Unix.file_perm -> (Unix.file_descr -> 'a) -> 'a

(** Functions for writing/reading to/from a file in a filesystem. *)

val write_file : Filepath.filepath -> string -> unit
val read_file : Filepath.filepath -> string

(** Functions for copying files. *)

(** [copy_file src dst] will copy file [src] to [dst]. *)
val copy_file : Filepath.filepath -> Filepath.filepath -> unit

(** [copy_to_dir src dst] fill copy file [src] in directory [dst]. *)
val copy_to_dir : Filepath.filepath -> Filepath.filepath -> unit

(** [mktemp_dir_in prefix] creates a temporary directory in the
    current working directory, whose name starts with [prefix] but is
    otherwise random. *)
val mktemp_dir_in : string -> Filepath.filepath
