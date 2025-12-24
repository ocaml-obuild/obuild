(** PPX and Syntax Preprocessor Resolution

    This module handles the resolution of PPX preprocessors and syntax extensions
    (like camlp4) for OCaml compilation targets.

    = Key Responsibilities =

    - Resolve camlp4/syntax preprocessor dependencies
    - Generate preprocessor flags for compilation
    - Handle both internal and external syntax packages
    - Support camlp4o and camlp4r syntax variants

    = Historical Context =

    OCaml has evolved through several preprocessing systems:
    - camlp4: Original extensible preprocessor (OCaml < 4.08)
    - PPX: Modern preprocessor system using AST transformations
    - This module supports both for compatibility
 *)

open Prepare_types

(** Get syntax preprocessor flags for a list of build dependencies

    @param bstate Build state containing configuration
    @param preprocessor The type of preprocessor (CamlP4O or CamlP4R)
    @param buildDeps List of build dependencies to process
    @return List of preprocessor flag lists
 *)
val get_syntax_pp : build_state -> Pp.Type.t -> Libname.t list -> string list list

(** Get target-specific preprocessor configuration

    @param bstate Build state containing configuration
    @param target The compilation target
    @param pp Optional preprocessor type
    @return Preprocessor configuration for the target
 *)
val get_target_pp : build_state -> Target.target -> Pp.Type.t option -> Pp.t
