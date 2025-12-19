(** Code generators for OCaml sources *)

exception GeneratorFailed of string
exception GeneratorNotFound of string

(** Generator configuration *)
type t = {
  suffix : string;
  (** File extension that triggers this generator *)

  modname : Modname.t -> Modname.t;
  (** Transform module name *)

  commands : Filepath.filepath -> Filepath.filepath -> string -> string list list;
  (** Generate command-line arguments for running the generator
      @param src Source filepath
      @param dest_root Destination root filepath
      @param moduleName Module name string
      @return List of command lists to execute *)

  generated_files : Filepath.filename -> string -> Filepath.filename;
  (** Determine output filename
      @param f Input filename
      @param moduleName Module name string
      @return Output filename *)
}

val get_all : unit -> t list
(** Get list of all registered generators *)

val is_generator_ext : string -> bool
(** Check if file extension has a registered generator *)

val get_generator : Filepath.filepath -> t
(** Get generator for filepath based on extension
    @raise GeneratorNotFound if no generator found *)

val run : Filepath.filepath -> Filepath.filepath -> string -> unit
(** Run generator for source file
    @param dest Destination filepath
    @param src Source filepath
    @param modName Module name
    @raise GeneratorFailed if generation fails *)
