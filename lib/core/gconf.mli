(** Global configuration *)

(** Verbosity levels *)
type verbosity_t =
  | Silent  (** No output *)
  | Report  (** Normal output *)
  | Verbose  (** Verbose output *)
  | Debug  (** Debug output *)
  | Trace  (** Debug with command output *)

type t = {
  mutable verbosity : verbosity_t;
  mutable parallel_jobs : int;
  mutable dump_dot : bool;
  mutable color : bool;
  mutable bin_annot : bool;
  mutable bin_annot_occurrences : bool;
  mutable short_path : bool;
  mutable ocamlmklib : bool;
  mutable ocaml_extra_args : string list;
}
(** Global configuration record *)

(** Typed target options *)
type target_option =
  | Executable_profiling
  | Executable_debugging
  | Executable_native
  | Executable_bytecode
  | Executable_as_obj
  | Library_profiling
  | Library_debugging
  | Library_native
  | Library_bytecode
  | Library_plugin
  | Build_benchs
  | Build_tests
  | Build_examples
  | Annot

val target_option_to_string : target_option -> string
val target_option_of_string : string -> target_option

exception UnknownOption of string

val gconf : t
(** Global configuration instance *)

val get_env : string -> string option
(** Get environment variable value *)

val set_env : string -> string -> unit
(** Set environment variable value *)

val get_target_option : string -> bool
(** Get target-specific option value by string key *)

val get_target_option_typed : target_option -> bool
(** Get target-specific option value by typed key *)

val set_target_options : string -> bool -> unit
(** Set target-specific option value by string key *)

val set_target_option_typed : target_option -> bool -> unit
(** Set target-specific option value by typed key *)

val get_target_options : unit -> (string * bool) list
(** Get all target options as string-keyed pairs *)

val get_target_options_keys : unit -> string list
(** Get all target option keys as strings *)
