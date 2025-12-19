(** Dependency analysis for OCaml and C code *)

(** Exception raised when OCaml dependency analysis fails *)
exception BuildDepAnalyzeFailed of string

(** Exception raised when C dependency analysis fails *)
exception BuildCDepAnalyzeFailed of string

(** Exception raised when a single dependency is missing *)
exception DependencyMissing of string

(** Exception raised when multiple dependencies are missing *)
exception DependenciesMissing of string list

(** Exception raised when dependency parsing fails *)
exception DependencyFailedParsing of string

(** OCaml library dependency with optional version constraint *)
type dependency = Libname.t * (Expr.t option)

(** C package dependency with optional version constraint *)
type cdependency = string * (Expr.t option)

(** Dependency analysis options *)
type dep_opt = {
  dep_includes : Filepath.filepath list;  (** Include paths for dependency analysis *)
  dep_pp       : Pp.t;                    (** Preprocessor to use *)
}

val run_ocamldep : dep_opt -> Filepath.filepath -> Modname.t list list
(** [run_ocamldep dopt src_file] analyzes OCaml module dependencies

    Runs ocamldep to determine which modules a source file depends on.
    Returns a list of module lists (for .ml and .mli if both exist).

    @param dopt dependency analysis options
    @param src_file source file to analyze
    @return list of module dependency lists
    @raise BuildDepAnalyzeFailed if ocamldep fails or returns invalid output *)

val run_ccdep : Filepath.filepath -> Filepath.filename list -> (Filepath.filename * Filepath.filepath list) list
(** [run_ccdep src_dir files] analyzes C file dependencies

    Runs gcc -MM to determine C file dependencies (headers, etc.).

    @param src_dir source directory containing C files
    @param files C source files to analyze
    @return list of (object_file, dependency_files) pairs
    @raise BuildCDepAnalyzeFailed if gcc dependency analysis fails *)
