(** Library name handling with support for hierarchical names

    This module provides support for library names in the form "abc" or "abc.def.xyz"
    where the main name can have multiple subpackage components. *)

(** The type of a library name with main name and optional subpackages *)
type t = {
  main_name : string;      (** The root package name *)
  subnames : string list;  (** List of subpackage names *)
}

(** {1 Exceptions} *)

exception EmptyLibName
(** Raised when attempting to create a library name from an empty string *)

(** {1 Construction and Conversion} *)

val of_string : string -> t
(** [of_string "abc.def.xyz"] creates a library name with
    main_name="abc" and subnames=["def"; "xyz"].
    @raise EmptyLibName if the string is empty *)

val to_string : t -> string
(** [to_string lname] converts library name to dot-separated string.
    Example: {main_name="abc"; subnames=["def"]} -> "abc.def" *)

val to_string_nodes : t -> string list
(** [to_string_nodes lname] returns list of all name components.
    Example: {main_name="abc"; subnames=["def"]} -> ["abc"; "def"] *)

val append : t -> string -> t
(** [append lname sub] adds a subpackage name to the library name *)

(** {1 Library File Extensions} *)

val to_libstring : t -> string
(** [to_libstring lname] converts to underscore-separated string.
    Example: "abc.def" -> "abc_def" *)

val to_cmxs : Types.ocaml_compilation_option -> t -> Filepath.filename
(** [to_cmxs opt lib] returns the dynamically loadable native plugin filename (.cmxs) *)

val to_cmxa : Types.ocaml_compilation_option -> t -> Filepath.filename
(** [to_cmxa opt lib] returns the native archive filename (.cmxa) *)

val to_cma : Types.ocaml_compilation_option -> t -> Filepath.filename
(** [to_cma opt lib] returns the bytecode archive filename (.cma) *)

val to_cmca : Types.ocaml_compiled_type -> Types.ocaml_compilation_option -> t -> Filepath.filename
(** [to_cmca compiled_type opt lib] returns the appropriate archive based on compilation type:
    - [Native] -> .cmxa
    - [ByteCode] -> .cma *)
