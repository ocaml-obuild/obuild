(** Exception handling and error reporting *)

val show : exn -> unit
(** Handle and display exception with appropriate error message and exit code.
    Matches against known exceptions from various modules (Project, Dist,
    Configure, Build, etc.) and prints formatted error messages before
    exiting with module-specific exit codes. *)
