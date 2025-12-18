(** META file caching for efficient library dependency resolution

    This module provides a caching layer for parsed META files, reducing
    redundant file I/O and parsing operations during dependency resolution. *)

(** {1 Cache Operations} *)

val get : string -> Meta.t
(** [get name] retrieves the parsed META file for library [name].
    If not cached, fetches from disk via {!Meta.find_lib} and caches the result.
    @raise Dependencies.DependencyMissing if library not found *)

val get_from_cache : Libname.t -> Meta.t
(** [get_from_cache lib] retrieves the ROOT package from cache for a library.

    IMPORTANT: This function always returns the root package, even if [lib]
    specifies subpackages via {!Libname.subnames}. Callers must use
    {!Meta.Pkg.find} to resolve subpackages themselves.

    @raise Failure if library not found in cache (indicates internal error) *)

val add : string -> Meta.t -> unit
(** [add name meta] adds a parsed META file to the cache.
    Typically used for synthetic META files (e.g., stdlib). *)

val find : string -> Meta.t option
(** [find name] looks up a library in the cache without fetching from disk.
    Returns [Some meta] if cached, [None] otherwise. *)
