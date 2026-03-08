(** Library and executable installation *)

val opam_install_file : Project.t -> (string * bool) list -> unit
(** [opam_install_file proj_file flags] generates an OPAM .install file.

    Creates a <project>.install file listing all files to be installed.

    @param proj_file the project configuration
    @param flags configuration flags for determining which targets are installable *)

val install : Project.t -> Filepath.filepath -> Filepath.filepath -> bool -> unit
(** [install_libs proj_file dest_dir bin_dir opam] installs libraries and executables.

    Copies compiled library files, META files, and executables to the installation directories.

    @param proj_file the project configuration
    @param dest_dir destination directory for library installation
    @param bin_dir destination directory for executable installation
    @param opam whether to use OPAM-style installation (generates .install file only) *)
