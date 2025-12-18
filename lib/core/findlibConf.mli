(** OCamlfind configuration file parsing

    This module handles reading and parsing OCamlfind/findlib configuration
    files to locate library search paths. *)

(** {1 Configuration Loading} *)

val load : unit -> unit
(** [load ()] loads the OCamlfind configuration from the system.

    Configuration sources (in priority order):
    1. User-specified path via [-findlib-path] flag
    2. [OCAMLFIND_CONF] environment variable
    3. Output of [ocamlfind printconf conf]
    4. Default paths: [/etc/findlib.conf] or [/etc/ocamlfind.conf]

    This function should be called before using {!get_paths}. *)

val get_paths : unit -> Filepath.filepath list
(** [get_paths ()] returns the library search paths from the loaded configuration.

    @return List of directory paths where OCaml libraries are installed

    Note: Call {!load} first to populate the configuration *)

val get_destdir : unit -> Filepath.filepath option
(** [get_destdir ()] returns the destination directory for library installation.

    @return [Some path] if destdir is configured, [None] otherwise

    Note: Call {!load} first to populate the configuration *)
