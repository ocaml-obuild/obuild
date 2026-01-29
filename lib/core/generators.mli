(** Code generators for OCaml sources

    Generators transform source files (e.g., .mly, .mll) into OCaml code.

    Generators with a suffix are automatically triggered during module discovery.
    Generators without a suffix must be used via explicit generate blocks. *)

exception GeneratorFailed of string
exception GeneratorNotFound of string

(** Internal generator representation for build system integration *)
type t = {
  suffix : string;
  (** File extension that triggers this generator (empty for generate-block-only) *)

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

(** Custom generator definition from .obuild file *)
type custom = {
  custom_name : string;                   (** Generator name for reference *)
  custom_suffix : string option;          (** File extension for automatic detection *)
  custom_command : string;                (** Command template with variables *)
  custom_outputs : string list;           (** Output file patterns *)
  custom_module_name : string option;     (** Module name pattern if different from base *)
}

val get_all : unit -> t list
(** Get list of all registered generators (built-in + custom) *)

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

(** {2 Custom Generator Registration} *)

val register_custom : custom -> unit
(** Register a custom generator from project file *)

val register_customs : custom list -> unit
(** Register multiple custom generators *)

val clear_custom_generators : unit -> unit
(** Clear all custom generators (useful for testing) *)

(** {2 Multi-Input Generators (for generate blocks)} *)

val run_custom_multi :
  generator_name:string ->
  dest:Filepath.filepath ->
  sources:Filepath.filepath list ->
  extra_args:string option ->
  unit
(** Run a generator with multiple input files (for generate blocks)
    @param generator_name Name of the generator to use
    @param dest Destination filepath (without extension)
    @param sources List of source files
    @param extra_args Additional command-line arguments
    @raise GeneratorFailed if generation fails
    @raise GeneratorNotFound if generator not found *)

val get_custom_outputs : custom -> src:Filepath.filepath -> Filepath.filename list
(** Get the output files for a custom generator given a source file *)

val find_generator_by_name : string -> custom option
(** Find a custom generator by name *)

(** {2 Variable Substitution} *)

val substitute_variables :
  src:Filepath.filepath ->
  dest:Filepath.filepath ->
  sources:Filepath.filepath list ->
  string -> string
(** Substitute variables in a command string.
    Variables: ${src}, ${dest}, ${base}, ${srcdir}, ${destdir}, ${sources} *)

val substitute_output_pattern :
  src:Filepath.filepath ->
  string -> string
(** Substitute variables in an output pattern.
    Variables: ${base} *)
