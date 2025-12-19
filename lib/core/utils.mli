(** Utility functions *)

exception FileNotFoundInPaths of (Filepath.filepath list * Filepath.filename)
exception FilesNotFoundInPaths of (Filepath.filepath list * Filepath.filepath list)

val read_file_with : (string -> 'a option) -> string -> 'a list
(** Read file line by line, applying function to each line
    @param f Function to transform each line (None skips the line)
    @param filename File to read
    @return List of transformed lines *)

val toKV : string -> string * string option
(** Parse line as colon-separated key-value pair *)

val toKVeq : string -> string * string option
(** Parse line as equals-separated key-value pair *)

val parseCSV : string -> string list
(** Parse comma-separated values *)

val to_include_path_options : Filepath.filepath list -> string list
(** Convert filepaths to -I include options, removing duplicates *)

val showList : string -> ('a -> string) -> 'a list -> string
(** Convert list to string with separator
    @param sep Separator string
    @param f Function to convert element to string
    @param l List to convert *)

val isWindows : bool
(** True if running on Windows platform *)

val to_exe_name : Types.ocaml_compilation_option -> Types.ocaml_compiled_type -> string -> Filepath.filename
(** Generate executable filename with appropriate extensions
    @param mode Compilation option (Normal, WithDebug, WithProf)
    @param build Build type (ByteCode, Native)
    @param name Base name *)

val get_system_paths : unit -> Filepath.filepath list
(** Get system PATH directories *)

val find_in_paths : Filepath.filepath list -> Filepath.filename -> Filepath.filepath
(** Find file in directory paths
    @raise FileNotFoundInPaths if not found *)

val find_choice_in_paths :
  Filepath.filepath list ->
  (Filepath.filepath -> Filepath.filepath) list ->
  Filepath.filepath
(** Find first path where one of the filename generators succeeds
    @raise FilesNotFoundInPaths if none found *)

val exist_choice_in_paths :
  Filepath.filepath list ->
  (Filepath.filepath -> Filepath.filepath) list ->
  bool
(** Check if any filename generator succeeds in any path *)

val find_in_system_path : Filepath.filename -> Filepath.filepath
(** Find file in system PATH
    @raise FileNotFoundInPaths if not found *)

val generateFile : Filepath.filepath -> ((string -> unit) -> unit) -> unit
(** Generate file using buffer-based writer
    @param file Output filepath
    @param f Function receiving string appender *)
