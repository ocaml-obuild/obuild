(** Core type definitions for OCaml compilation modes and options *)

(** Compilation optimization option *)
type ocaml_compilation_option =
  | Normal     (** Standard compilation *)
  | WithDebug  (** Compilation with debugging symbols *)
  | WithProf   (** Compilation with profiling instrumentation *)

(** Compilation target type *)
type ocaml_compiled_type =
  | ByteCode   (** OCaml bytecode compilation *)
  | Native     (** Native code compilation *)

(** Compilation mode - interface or compiled code *)
type ocaml_compilation_mode =
  | Interface                        (** Interface files (.mli/.cmi) *)
  | Compiled of ocaml_compiled_type  (** Compiled implementation *)

(** [extDP opt] returns the file extension suffix for a compilation option.
    - [Normal] -> ""
    - [WithDebug] -> ".d"
    - [WithProf] -> ".p" *)
val extDP : ocaml_compilation_option -> string
