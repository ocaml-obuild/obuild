(** Source distribution creation *)

val run : Project.t -> bool -> unit
(** [run proj_file is_snapshot] creates a source distribution tarball.

    Creates a .tar.gz file containing all project sources in the dist directory.

    @param proj_file the project configuration
    @param is_snapshot whether this is a snapshot release (currently unused) *)
