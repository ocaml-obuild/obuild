(** Documentation generation *)

exception DocumentationBuildingFailed of string
(** Raised when documentation building fails with an error message *)

val run : Project.t -> unit
(** [run proj_file] generates documentation for the project.

    Currently a stub implementation.

    @raise DocumentationBuildingFailed if documentation generation fails *)
