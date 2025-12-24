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

open Fugue
open Filepath
open Analyze
open Types
open Helper
open Gconf
open Target
open Prepare_types

(* Camlp4 library identifier *)
let camlp4Libname = Libname.of_string "camlp4"

(* Common predicates for syntax preprocessing *)
let syntaxPredsCommon = [Meta.Predicate.Syntax; Meta.Predicate.Preprocessor]

(* Get the camlp4 predicate based on preprocessor type *)
let get_p4pred = function
  | Pp.Type.CamlP4O -> Meta.Predicate.Camlp4o
  | Pp.Type.CamlP4R -> Meta.Predicate.Camlp4r

(** Get syntax preprocessor flags for a list of build dependencies *)
let get_syntax_pp bstate preprocessor buildDeps =
  let conf = bstate.bstate_config in
  let p4pred = get_p4pred preprocessor in
  let stdlib = fp (get_ocaml_config_key "standard_library" conf) in
  list_filter_map (fun spkg ->
      if Analyze.is_pkg_internal conf spkg
      then (
        let lib = Project.find_lib bstate.bstate_config.project_file spkg in
        if lib.Project.Library.syntax
        then (
          (* TODO need to make sure that the bytecode option has been enabled for the syntax library *)
          let dir = Dist.get_build_exn (Dist.Target (Name.Lib lib.Project.Library.name)) in
          Some [fp_to_string (dir </> Libname.to_cmca ByteCode Normal lib.Project.Library.name) ]
        ) else None
      ) else (
        let meta = Metacache.get_from_cache spkg in
        let preds =
          if spkg = camlp4Libname
          then p4pred :: syntaxPredsCommon
          else syntaxPredsCommon
        in
        if Meta.Pkg.is_syntax meta spkg
        then (
          let includePath = Meta.get_include_dir stdlib meta in
          Some ["-I"; fp_to_string includePath; Meta.Pkg.get_archive meta spkg preds]
        ) else
          None
      )
    ) buildDeps

(** Get target-specific preprocessor configuration *)
let get_target_pp bstate target = function
  | None    -> Pp.none
  | Some pp ->
    let conf = bstate.bstate_config in
    let nodes = List.rev (Taskdep.linearize conf.project_pkgdeps_dag Taskdep.FromParent
                            [Analyze.Target target.target_name]) in
    let syntaxPkgs = list_filter_map (fun node ->
        match node with
        | Dependency dep -> Some dep
        | _              -> None
      ) nodes
    in
    verbose Verbose " all packages : [%s]\n%!" (Utils.showList "," Libname.to_string syntaxPkgs);
    let p4pred = get_p4pred pp in
    let p4Meta = Metacache.get_from_cache camlp4Libname in
    let preproc = (snd p4Meta).Meta.Pkg.preprocessor in
    let archive = [Meta.Pkg.get_archive p4Meta camlp4Libname (p4pred::syntaxPredsCommon)] in
    (*verbose Verbose " camlp4 strs: [%s]\n%!" (Utils.showList "] [" id camlp4Strs);*)
    let camlp4Strs = get_syntax_pp bstate pp syntaxPkgs in
    Pp.some preproc (archive :: camlp4Strs)
