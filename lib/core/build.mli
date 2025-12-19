(** Build orchestration and compilation *)

(** Exception raised when C compilation fails *)
exception CCompilationFailed of string

(** Exception raised when OCaml compilation fails *)
exception CompilationFailed of string

(** Exception raised for internal consistency errors *)
exception Internal_Inconsistancy of string * string

val build_exe : Prepare.build_state -> Project.Executable.t -> unit
(** [build_exe bstate exe] builds a single executable

    Prepares compilation state and compiles the executable
    with all its dependencies.

    @param bstate global build state
    @param exe executable to build
    @raise CCompilationFailed if C compilation fails
    @raise CompilationFailed if OCaml compilation fails *)

val build_dag : Prepare.build_state -> Project.t -> Target.Name.t Dag.t -> unit
(** [build_dag bstate proj_file targets_dag] builds targets according to DAG

    Orchestrates parallel compilation of multiple targets
    respecting dependency order defined in the DAG.

    @param bstate global build state
    @param proj_file project configuration
    @param targets_dag target dependency DAG
    @raise CCompilationFailed if C compilation fails
    @raise CompilationFailed if OCaml compilation fails *)

val sanity_check : Filepath.filepath -> Target.target -> unit
(** [sanity_check build_dir target] verifies all target output files exist

    Checks that all expected compilation outputs are present
    in the build directory. Logs warnings for missing files.

    @param build_dir target build directory
    @param target target to check *)
