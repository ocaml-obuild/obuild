(** Build program execution *)

exception LinkingFailed of string
exception InferFailed of string

(** C linking mode *)
type c_linking_mode =
  | LinkingStatic   (** Static linking *)
  | LinkingShared   (** Shared linking *)

(** OCaml linking mode *)
type linking_mode =
  | LinkingLibrary      (** Library linking *)
  | LinkingPlugin       (** Plugin linking *)
  | LinkingExecutable   (** Executable linking *)

(** Annotation mode for compiled files *)
type annotation_mode =
  | AnnotationNone  (** No annotations *)
  | AnnotationBin   (** Binary annotations *)
  | AnnotationText  (** Text annotations *)
  | AnnotationBoth  (** Both binary and text annotations *)

(** Pack option *)
type packopt = Hier.t option

val run_ocaml_compile :
  Prepare.dir_spec ->
  Prepare.use_thread_flag ->
  annotation_mode ->
  Types.ocaml_compilation_mode ->
  Types.ocaml_compilation_option ->
  packopt ->
  Pp.t ->
  string list ->
  Hier.t ->
  Process.t
(** Compile an OCaml module *)

val run_ocaml_pack :
  Filepath.filepath ->
  Filepath.filepath ->
  annotation_mode ->
  Types.ocaml_compiled_type ->
  packopt ->
  Hier.t ->
  Hier.t list ->
  Process.t
(** Pack multiple OCaml modules into one *)

val run_ocaml_infer :
  Filepath.filepath ->
  Filepath.filepath list ->
  Pp.t ->
  Hier.t ->
  string
(** Infer OCaml interface from implementation *)

val o_from_cfile : Filepath.filename -> Filepath.filename
(** Convert C filename to object filename *)

val run_c_compile :
  Analyze.project_config ->
  Prepare.dir_spec ->
  string list ->
  Filepath.filename ->
  Process.t
(** Compile a C file *)

val run_ar :
  Filepath.filepath ->
  Filepath.filepath list ->
  Process.t
(** Create static archive *)

val run_ranlib :
  Filepath.filepath ->
  Process.t
(** Run ranlib on static archive *)

val run_c_linking :
  c_linking_mode ->
  Filepath.filepath list ->
  Filepath.filepath ->
  Process.t
(** Link C object files *)

val run_ocaml_linking :
  Filepath.filepath list ->
  Types.ocaml_compiled_type ->
  linking_mode ->
  Types.ocaml_compilation_option ->
  Prepare.thread_type ->
  string ->
  string list ->
  Filepath.filepath list ->
  Hier.t list ->
  Filepath.filepath ->
  Process.t
(** Link OCaml modules into library or executable *)
