(** OCaml toolchain and external program paths *)

(** Exception raised when an OCaml program fails *)
exception OCamlProgramError of string

(** Exception raised when tar command fails *)
exception TarError of string

(** Exception raised when pkg-config command fails *)
exception PkgConfigError of string

(** Exception raised when pkg-config doesn't return version *)
exception PkgConfigErrorNoVersion

(** Exception raised when pkg-config output is unexpected *)
exception PkgConfigErrorUnexpectedOutput of string

(** Exception raised when a required program is not found *)
exception ProgramNotFound of string

(** OCaml toolchain program getters *)

val get_ocamlopt : unit -> string
(** Get path to ocamlopt (native compiler) *)

val get_ocamlc : unit -> string
(** Get path to ocamlc (bytecode compiler) *)

val get_ocamldep : unit -> string
(** Get path to ocamldep (dependency analyzer) *)

val get_ocamldoc : unit -> string
(** Get path to ocamldoc (documentation generator) *)

val get_ocamlmklib : unit -> string
(** Get path to ocamlmklib (library linker) *)

val get_camlp4 : unit -> string
(** Get path to camlp4 (preprocessor) *)

val get_cc : unit -> string
(** Get path to C compiler (gcc) *)

val get_ranlib : unit -> string
(** Get path to ranlib *)

val get_ar : unit -> string
(** Get path to ar (archiver) *)

val get_ld : unit -> string
(** Get path to ld (linker) *)

val get_pkg_config : unit -> string
(** Get path to pkg-config *)

val get_ocaml : unit -> string
(** Get path to ocaml (toplevel) *)

val get_ocamlmktop : unit -> string
(** Get path to ocamlmktop (custom toplevel builder) *)

(** OCaml configuration *)

val get_ocaml_version : (string, string) Hashtbl.t -> string * string * string
(** [get_ocaml_version cfg] extracts OCaml version as (major, minor, other)

    @param cfg OCaml configuration hashtable
    @return (major, minor, other) version components
    @raise OCamlProgramError if version format is unexpected *)

val get_ocaml_config : unit -> (string, string) Hashtbl.t
(** [get_ocaml_config ()] retrieves OCaml compiler configuration

    Runs "ocamlc -config" and caches the result.

    @return configuration hashtable
    @raise OCamlProgramError if ocamlc fails *)

val get_camlp4_config : unit -> string list
(** [get_camlp4_config ()] retrieves camlp4 library paths

    Runs "camlp4 -where" to get the installation directories.

    @return list of camlp4 library paths
    @raise OCamlProgramError if camlp4 fails *)

(** External tool invocations *)

val run_tar : string -> string -> unit
(** [run_tar output dir] creates a tar.gz archive

    @param output output filename
    @param dir directory to archive
    @raise TarError if tar command fails *)

val run_pkg_config_version : string -> string
(** [run_pkg_config_version name] gets package version from pkg-config

    @param name package name
    @return version string
    @raise PkgConfigError if pkg-config fails
    @raise PkgConfigErrorNoVersion if no version returned
    @raise PkgConfigErrorUnexpectedOutput if output format is unexpected *)

val run_pkg_config_includes : string -> string list
(** [run_pkg_config_includes name] gets include flags from pkg-config

    @param name package name
    @return list of include paths (without -I prefix)
    @raise PkgConfigError if pkg-config fails *)

val run_pkg_config_libs : string -> string list
(** [run_pkg_config_libs name] gets library flags from pkg-config

    @param name package name
    @return list of library names (without -l prefix)
    @raise PkgConfigError if pkg-config fails *)
