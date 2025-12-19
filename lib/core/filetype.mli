(** File type identification and manipulation *)

(** File type enumeration *)
type t =
  | FileML        (** OCaml implementation file (.ml) *)
  | FileMLI       (** OCaml interface file (.mli) *)
  | FileH         (** C header file (.h) *)
  | FileC         (** C source file (.c) *)
  | FileCMX       (** Native compiled object file (.cmx) *)
  | FileCMO       (** Bytecode compiled object file (.cmo) *)
  | FileCMI       (** Compiled interface file (.cmi) *)
  | FileCMA       (** Bytecode library archive (.cma) *)
  | FileCMXA      (** Native library archive (.cmxa) *)
  | FileCMXS      (** Native plugin (.cmxs) *)
  | FileCMT       (** Compiled typed abstract syntax tree (.cmt) *)
  | FileCMTI      (** Compiled interface typed abstract syntax tree (.cmti) *)
  | FileO         (** Object file (.o) *)
  | FileA         (** Static archive (.a) *)
  | FileSO        (** Shared object (.so) *)
  | FileEXE       (** Executable (.exe) *)
  | FileOther of string  (** Other file type with extension *)

val of_string : string -> t
(** Parse file type from extension string *)

val to_string : t -> string
(** Convert file type to extension string *)

(** File dependency identifier combining type and path *)
type id = {
  fdep_ty : t;                  (** File type *)
  fdep_path : Filepath.filepath;  (** File path *)
}

val make_id : t * Filepath.filepath -> id
(** Create file dependency identifier from type and path *)

val get_id : id -> t * Filepath.filepath
(** Extract type and path from file dependency identifier *)

val get_type : id -> t
(** Get file type from identifier *)

val get_path : id -> Filepath.filepath
(** Get file path from identifier *)

val of_filename : Filepath.filename -> t
(** Determine file type from filename extension *)

val of_filepath : Filepath.filepath -> t
(** Determine file type from filepath extension *)

val replace_extension : Filepath.filename -> t -> Filepath.filename
(** Replace filename extension with given file type *)

val replace_extension_path : Filepath.filepath -> t -> Filepath.filepath
(** Replace filepath extension with given file type *)
