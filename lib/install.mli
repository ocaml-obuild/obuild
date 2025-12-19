(** Library and executable installation *)

val opam_install_file : Project.t -> (string * bool) list -> unit
(** [opam_install_file proj_file flags] generates an OPAM .install file.

    Creates a <project>.install file listing all files to be installed.

    @param proj_file the project configuration
    @param flags configuration flags for determining which targets are installable *)

val install_libs : Project.t -> Filepath.filepath -> bool -> unit
(** [install_libs proj_file dest_dir opam] installs libraries to the destination directory.

    Copies compiled library files and META files to the installation directory.

    @param proj_file the project configuration
    @param dest_dir destination directory for installation
    @param opam whether to use OPAM-style installation *)
