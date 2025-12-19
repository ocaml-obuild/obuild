(** Project configuration management *)

(** Exception raised when configuration has changed *)
exception ConfigChanged of string

(** Exception raised when a required external tool is not found *)
exception ToolNotFound of Filepath.filename

(** Exception raised when a configuration key is missing *)
exception ConfigurationMissingKey of string

(** Exception raised when a configuration value has wrong type *)
exception ConfigurationTypeMismatch of string * string * string

(** Exception raised when a configure script fails *)
exception ConfigureScriptFailed of string

(** Flag manipulation actions *)
type flag_action =
  | SetFlag of string     (** Enable a flag *)
  | ClearFlag of string   (** Disable a flag *)

val run : Project.t -> flag_action list -> (string * bool) list -> unit
(** [run proj_file user_flags user_opts] configures the project

    Performs configuration including:
    - Checking OCaml version and capabilities
    - Executing configure script if present
    - Processing user flags and options
    - Creating build directories
    - Generating auto-configuration files
    - Writing setup file

    @param proj_file the project to configure
    @param user_flags flags to set or clear
    @param user_opts additional options to set *)

val check : Project.t -> bool -> (string, string) Hashtbl.t -> (string * bool) list
(** [check proj_file reconf setup] checks configuration and potentially reconfigures

    Validates that:
    - OCaml configuration hasn't changed
    - Project file digest matches
    - Returns the configured flags

    @param proj_file the project to check
    @param reconf whether to force reconfiguration on digest change
    @param setup the existing setup hashtable
    @return list of (flag_name, flag_value) pairs *)

val set_opts : (string, string) Hashtbl.t -> unit
(** [set_opts hashtable] sets target options from configuration hashtable *)
