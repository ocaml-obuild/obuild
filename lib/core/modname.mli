(** Module name handling and validation

    This module provides types and functions for working with OCaml module names,
    ensuring they follow naming conventions (capitalized, valid characters). *)

(** The type of a module name (internally represented as a string) *)
type t

(** {1 Exceptions} *)

exception InvalidModuleName of string
(** Raised when a string doesn't satisfy module name requirements *)

exception EmptyModuleName
(** Raised when attempting to create a module name from an empty string *)

exception ModuleFilenameNotValid of string
(** Raised when a filename cannot be converted to a valid module name *)

(** {1 Validation Helpers} *)

val char_is_valid_modchar : char -> bool
(** [char_is_valid_modchar c] returns [true] if [c] is a valid character
    in a module name (letter, digit, or underscore) *)

val string_all : (char -> bool) -> string -> bool
(** [string_all p s] returns [true] if predicate [p] holds for all
    characters in string [s] *)

(** {1 Construction and Conversion} *)

val wrap : string -> t
(** [wrap s] creates a module name from string [s].
    @raise InvalidModuleName if [s] contains invalid characters or isn't capitalized
    @raise EmptyModuleName if [s] is empty *)

val of_string : string -> t
(** [of_string s] is an alias for [wrap s] *)

val to_string : t -> string
(** [to_string m] converts module name [m] to a string *)

val of_directory : Filepath.filename -> t
(** [of_directory fn] creates a module name from a directory name,
    capitalizing it appropriately.
    @raise InvalidModuleName if the filename is invalid
    @raise EmptyModuleName if the filename is empty *)

val of_filename : Filepath.filename -> t
(** [of_filename fn] creates a module name from a filename,
    removing the extension and capitalizing.
    @raise ModuleFilenameNotValid if the filename cannot be converted *)

(** {1 File Extensions and Paths} *)

val to_dir : t -> string
(** [to_dir m] converts module name to directory name (uncapitalized) *)

val to_o : t -> Filepath.filename
(** [to_o m] converts module name to object file (.o) *)

val to_directory : t -> Filepath.filename
(** [to_directory m] converts module name to directory filename *)

val to_filename : t -> Filepath.filename
(** [to_filename m] converts module name to source file (.ml) *)

