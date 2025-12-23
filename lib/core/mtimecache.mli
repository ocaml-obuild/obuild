(** Modification time caching to reduce filesystem operations

    During a build, obuild frequently checks file modification times to determine
    what needs rebuilding. Many files are checked multiple times (e.g., .cmi files
    used by multiple modules). This cache reduces redundant stat() system calls.

    The cache is invalidated for files after they are written/modified.
*)

type t
(** Cache of file modification times *)

val create : unit -> t
(** Create a new empty cache *)

val get_mtime : t -> Filepath.filepath -> float
(** Get modification time for file, using cache if available.
    If file doesn't exist, returns 0.0.
    Caches the result for future calls. *)

val invalidate : t -> Filepath.filepath -> unit
(** Invalidate cached mtime for a file (e.g., after writing) *)

val clear : t -> unit
(** Clear all cached mtimes and reset statistics *)

val stats : t -> int * int
(** Returns (hits, misses) for cache performance analysis.
    - hits: number of times mtime was found in cache
    - misses: number of times stat() was called *)
