(** Simple, portable command-line interface library

    This library provides a clean API for building CLI applications with subcommands,
    automatic help generation, and good error messages. It requires no external
    dependencies and works with OCaml 4.01+.

    {2 Example Usage}

    {[
      open Cli

      let build_cmd = command "build"
        ~doc:"Compile the project"
        ~args:[
          flag "verbose" ~short:'v' ~doc:"Show detailed output";
          option_int "jobs" ~short:'j' ~doc:"Number of parallel jobs";
        ]
        ~run:(fun ctx ->
          let verbose = get_flag ctx "verbose" in
          let jobs = get_int ctx "jobs" ~default:2 in
          Printf.printf "Building with %d jobs (verbose=%b)\\n" jobs verbose
        )

      let cli = app "myapp"
        ~version:"1.0.0"
        ~doc:"My awesome application"
        ~commands:[build_cmd]

      let () = run cli
    ]}
*)

(** {1 Core Types} *)

(** Execution context containing parsed arguments *)
type context

(** Command specification *)
type command

(** Application specification *)
type app

(** {1 Argument Specifications} *)

(** Flag argument (boolean, no value required) *)
val flag :
  string ->
  ?short:char ->
  ?env:string ->
  doc:string ->
  command -> command
(** [flag name ~short ~env ~doc cmd] adds a boolean flag to command [cmd].
    - [name]: Long name (e.g., "verbose" for --verbose)
    - [short]: Optional short name (e.g., 'v' for -v)
    - [env]: Optional environment variable name to read default from
    - [doc]: Help documentation string *)

(** String option (requires a value) *)
val option_string :
  string ->
  ?short:char ->
  ?env:string ->
  ?default:string ->
  ?placeholder:string ->
  doc:string ->
  command -> command
(** [option_string name ~short ~env ~default ~placeholder ~doc cmd] adds a string option.
    - [placeholder]: Shown in help (e.g., "PATH" for --output PATH) *)

(** Integer option (requires an integer value) *)
val option_int :
  string ->
  ?short:char ->
  ?env:string ->
  ?default:int ->
  ?placeholder:string ->
  doc:string ->
  command -> command

(** Boolean option (requires true/false/yes/no/1/0 value) *)
val option_bool :
  string ->
  ?short:char ->
  ?env:string ->
  ?default:bool ->
  ?placeholder:string ->
  doc:string ->
  command -> command
(** [option_bool name ~short ~env ~default ~placeholder ~doc cmd] adds a boolean option.
    - Accepts values: true/false, yes/no, 1/0, on/off (case insensitive)
    - [placeholder]: Shown in help (default: "true|false") *)

(** String list option (can be specified multiple times) *)
val option_strings :
  string ->
  ?short:char ->
  ?env:string ->
  ?placeholder:string ->
  doc:string ->
  command -> command
(** Values accumulate when flag is used multiple times *)

(** Positional arguments *)
val positional :
  string ->
  ?placeholder:string ->
  doc:string ->
  command -> command
(** [positional name ~placeholder ~doc cmd] adds a required positional argument *)

val positionals :
  string ->
  ?placeholder:string ->
  doc:string ->
  command -> command
(** [positionals name ~placeholder ~doc cmd] adds optional positional arguments (list) *)

(** {1 Command Construction} *)

val command :
  string ->
  doc:string ->
  ?description:string ->
  ?args:(command -> command) list ->
  run:(context -> unit) ->
  unit ->
  command
(** [command name ~doc ~description ~args ~run] creates a command.
    - [name]: Command name (e.g., "build")
    - [doc]: Short one-line documentation
    - [description]: Optional longer description shown in help
    - [args]: List of argument specifications
    - [run]: Function to execute with parsed context *)

val command_with_subcommands :
  string ->
  doc:string ->
  ?description:string ->
  commands:command list ->
  command
(** [command_with_subcommands name ~doc ~description ~commands] creates a command
    that has its own subcommands (e.g., "git remote add") *)

(** {1 Application Construction} *)

val app :
  string ->
  version:string ->
  doc:string ->
  ?description:string ->
  ?global_args:(command -> command) list ->
  ?on_global_args:(context -> unit) ->
  commands:command list ->
  unit ->
  app
(** [app name ~version ~doc ~description ~global_args ~on_global_args ~commands] creates the application.
    - [name]: Application name
    - [version]: Version string
    - [doc]: Short description
    - [description]: Longer description
    - [global_args]: Global flags/options available to all commands
    - [on_global_args]: Optional callback invoked with parsed global args before command execution
    - [commands]: List of commands *)

(** {1 Execution} *)

val run : ?argv:string array -> app -> unit
(** [run ~argv app] parses arguments and executes the appropriate command.
    If [argv] is not provided, uses [Sys.argv].
    Exits with code 0 on success, non-zero on error. *)

val run_result : ?argv:string array -> app -> (unit, string) Compat.result
(** [run_result ~argv app] same as [run] but returns a result instead of exiting.
    Useful for testing. *)

(** {1 Context Accessors} *)

(** Get boolean flag value *)
val get_flag : context -> string -> bool

(** Get optional string value *)
val get_string_opt : context -> string -> string option

(** Get string value with default *)
val get_string : context -> string -> default:string -> string

(** Get optional integer value *)
val get_int_opt : context -> string -> int option

(** Get integer value with default *)
val get_int : context -> string -> default:int -> int

(** Get optional boolean value *)
val get_bool_opt : context -> string -> bool option

(** Get boolean value with default *)
val get_bool : context -> string -> default:bool -> bool

(** Get list of strings (from repeated options or positionals) *)
val get_strings : context -> string -> string list

(** Get positional arguments *)
val get_positionals : context -> string list

(** {1 Error Handling} *)

exception Parse_error of string
(** Raised when argument parsing fails *)

exception Validation_error of string
(** Raised when argument validation fails *)

(** {1 Error Handling Utilities} *)

val format_error : string -> string
(** [format_error msg] formats an error message with context and suggestions *)

val format_suggestion : string list -> string
(** [format_suggestion similar] formats a "Did you mean" suggestion *)

(** {1 Utilities} *)

val version_flag : command -> command
(** Adds standard --version flag *)

val help_flag : command -> command
(** Adds standard -h/--help flag *)

val verbose_flag : command -> command
(** Adds standard -v/--verbose flag *)

val quiet_flag : command -> command
(** Adds standard -q/--quiet flag *)

(** {1 Advanced Features} *)

val set_exit_code : int -> unit
(** Set the exit code (default: 0 for success, 1 for error) *)

val get_command_name : context -> string
(** Get the name of the currently executing command *)

val print_help : app -> command option -> unit
(** Print help text for the app or a specific command *)

val suggest_command : app -> string -> string list
(** [suggest_command app name] returns similar command names using fuzzy matching *)

(** {1 Shell Completion} *)

val generate_bash_completion : app -> string
(** [generate_bash_completion app] generates a bash completion script *)

val generate_zsh_completion : app -> string
(** [generate_zsh_completion app] generates a zsh completion script *)

val generate_fish_completion : app -> string
(** [generate_fish_completion app] generates a fish completion script *)

(** {1 Configuration File Support} *)

type config
(** Configuration loaded from config files *)

val load_config : ?paths:string list -> unit -> config
(** [load_config ~paths ()] loads configuration from files.
    If [paths] is not provided, searches in default locations:
    - ./.obuildrc (project-specific config)
    - ~/.obuildrc (user config)
    Project-specific config takes precedence over user config. *)

val config_get_string : config -> string -> string option
(** [config_get_string config key] retrieves a string value from config *)

val config_get_int : config -> string -> int option
(** [config_get_int config key] retrieves an integer value from config *)

val config_get_bool : config -> string -> bool option
(** [config_get_bool config key] retrieves a boolean value from config *)

val run_with_config : ?argv:string array -> ?config:config -> app -> unit
(** [run_with_config ~argv ~config app] runs the app with config file defaults.
    Config values are applied as defaults before parsing command-line arguments.
    Command-line arguments override config file values. *)
