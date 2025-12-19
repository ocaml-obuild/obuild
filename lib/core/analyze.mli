(** Project analysis and dependency resolution *)

(** Exception raised when a sublibrary doesn't exist *)
exception SublibraryDoesntExists of Libname.t

(** Exception raised when an OCaml configuration key is missing *)
exception OcamlConfigMissing of string

(** Dependency origin type *)
type dep_type =
  | System    (** External system dependency *)
  | Internal  (** Internal project dependency *)

(** Dependency graph node tags *)
type dependency_tag =
  | Target of Target.Name.t       (** Build target node *)
  | Dependency of Libname.t        (** Library dependency node *)

(** C package configuration from pkg-config *)
type cpkg_config = {
  cpkg_conf_libs : string list;          (** Linker flags *)
  cpkg_conf_includes : Filepath.filepath list;  (** Include paths *)
}

(** Analyzed project configuration with resolved dependencies *)
type project_config = {
  project_dep_data : (Libname.t, dep_type) Hashtbl.t;
    (** Mapping of dependencies to their type (system/internal) *)
  project_pkgdeps_dag : dependency_tag Dag.t;
    (** Full dependency graph including targets and libraries *)
  project_targets_dag : Target.Name.t Dag.t;
    (** Internal target dependency graph *)
  project_all_deps : Dependencies.dependency list;
    (** All dependencies across all targets *)
  project_file : Project.t;
    (** Original project configuration *)
  project_ocamlcfg : (string, string) Hashtbl.t;
    (** OCaml compiler configuration *)
  project_ocamlmkcfg : (string, string) Hashtbl.t;
    (** OCaml makefile configuration *)
  project_cpkgs : (string, cpkg_config) Hashtbl.t;
    (** C package configurations *)
}

val prepare : Project.t -> (string * bool) list -> project_config
(** [prepare proj_file user_flags] analyzes project and resolves dependencies

    Performs full dependency analysis:
    - Resolves all library dependencies recursively
    - Builds dependency DAGs
    - Initializes META cache with standard library
    - Validates dependency constraints
    - Processes C package dependencies

    @param proj_file the project to analyze
    @param user_flags configured project flags
    @return analyzed project configuration
    @raise Dependencies.DependencyMissing if a dependency is not found
    @raise Dependencies.DependenciesMissing if multiple dependencies are missing
    @raise SublibraryDoesntExists if a sublibrary is not found *)

val get_ocaml_config_key_global : string -> string
(** [get_ocaml_config_key_global key] retrieves OCaml config value from global config

    @raise OcamlConfigMissing if key not found *)

val get_ocaml_config_key : string -> project_config -> string
(** [get_ocaml_config_key key project] retrieves OCaml config value from project

    @raise OcamlConfigMissing if key not found *)

val get_pkg_deps : Target.target -> project_config -> Libname.t list
(** [get_pkg_deps target project] gets all package dependencies for a target

    Returns dependencies in topological order *)

val get_c_pkg : string -> project_config -> cpkg_config
(** [get_c_pkg cname project] retrieves C package configuration

    @raise Failure if C package not found *)

val is_pkg_internal : project_config -> Libname.t -> bool
(** [is_pkg_internal project pkg] checks if package is internal to project *)

val is_pkg_system : project_config -> Libname.t -> bool
(** [is_pkg_system project pkg] checks if package is a system dependency *)

val get_internal_library_deps : project_config -> Target.target -> Libname.t list
(** [get_internal_library_deps project target] gets internal library dependencies

    Returns only dependencies that are libraries defined in the project *)
