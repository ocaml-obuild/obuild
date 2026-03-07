(** Distribution directory management *)

(** Build directory type *)
type t =
  | Autogen           (** Auto-generated files *)
  | Dot               (** DOT graph files *)
  | Target of Target.Name.t  (** Target-specific directory *)

val to_string : t -> string
(** Convert build directory type to string *)

exception DistNotADirectory
exception MissingDestinationDirectory of t
exception DistNotFound
exception DistFileNotFound of string

val set_path : Filepath.filepath -> unit
val get_path : unit -> Filepath.filepath
val build_path : unit -> Filepath.filepath
val setup_path : unit -> Filepath.filepath
val configure_path : unit -> Filepath.filepath

val check_exn : (unit -> unit) -> unit
(** Check dist directory exists, call function if it doesn't *)

val exist : unit -> unit
(** Check dist directory exists, raise DistNotFound if not *)

val create_maybe : unit -> unit
(** Create dist directory if it doesn't exist *)

val get_build : unit -> Filepath.filepath
val get_build_path : t -> Filepath.filepath
val get_build_exn : t -> Filepath.filepath
(** Get build path, raise MissingDestinationDirectory if doesn't exist *)

val create_build : t -> Filepath.filepath
(** Create and return build directory for given type *)

val read_setup : unit -> (string, string) Hashtbl.t
val read_configure : unit -> (string, string) Hashtbl.t
val write_setup : (string, string) Hashtbl.t -> unit

val remove_dead_links : unit -> unit
(** Remove symlinks pointing to build directories *)
