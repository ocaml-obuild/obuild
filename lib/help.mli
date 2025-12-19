(** Help messages for obuild commands *)

val help_configure : string list
(** Help text for the configure command *)

val help_clean : string list
(** Help text for the clean command *)

val help_build : string list
(** Help text for the build command *)

val help_sdist : string list
(** Help text for the sdist command *)

val help_messages : (string * string list) list
(** Association list of command names to their help text *)
