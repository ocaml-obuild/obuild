(** Preprocessor configuration *)

exception InvalidPreprocessor of string

(** Preprocessor package (list of package arguments) *)
type package = string list

(** Preprocessor type module *)
module Type : sig
  type t =
    | CamlP4O  (** Camlp4o preprocessor *)
    | CamlP4R  (** Camlp4r (revised syntax) preprocessor *)

  val of_string : string -> t
  (** Parse preprocessor type from string *)

  val to_string : t -> string
  (** Convert preprocessor type to string *)
end

(** Preprocessor descriptor *)
type desc = {
  camlp4 : string;           (** Preprocessor command *)
  packages : package list;   (** Package arguments *)
}

(** Preprocessor option (may be None if no preprocessing) *)
type t = desc option

val some : string -> package list -> t
(** Create a preprocessor option with command and packages *)

val none : t
(** No preprocessor *)

val append : t -> package list -> t
(** Append packages to preprocessor configuration *)

val to_params : t -> string list
(** Convert preprocessor to command-line parameters *)
