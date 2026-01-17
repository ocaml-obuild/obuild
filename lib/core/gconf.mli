(** Global configuration *)

(** Verbosity levels *)
type verbosity_t =
  | Silent  (** No output *)
  | Report  (** Normal output *)
  | Verbose  (** Verbose output *)
  | Debug  (** Debug output *)
  | DebugPlus  (** Debug with command output *)

type t = {
  mutable verbosity : verbosity_t;
  mutable parallel_jobs : int;
  mutable dump_dot : bool;
  mutable color : bool;
  mutable bin_annot : bool;
  mutable short_path : bool;
  mutable ocamlmklib : bool;
  mutable ocaml_extra_args : string list;
}
(** Global configuration record *)

exception UnknownOption of string

val gconf : t
(** Global configuration instance *)

val get_env : string -> string option
(** Get environment variable value *)

val set_env : string -> string -> unit
(** Set environment variable value *)

val get_target_option : string -> bool
(** Get target-specific option value *)

val set_target_options : string -> bool -> unit
(** Set target-specific option value *)

val get_target_options : unit -> (string * bool) list
(** Get all target options *)

val get_target_options_keys : unit -> string list
(** Get all target option keys *)
