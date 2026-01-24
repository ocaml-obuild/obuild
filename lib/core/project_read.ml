(** New parser integration for Project reading

    This module provides an alternative to Project.read that uses the new parser (obuild_lexer ->
    obuild_parser -> obuild_validate).

    It's in a separate module to avoid cyclic dependencies between Project and Obuild_validate. *)

open Filepath

(** Read project file using the new parser *)
let read () =
  let path = Project.findPath () in
  let proj =
    try Obuild_validate.parse_and_convert_file (fp_to_string path) with
    | Obuild_validate.Validation_error (loc, msg) ->
        raise
          (Project.InvalidConfFile
             (Printf.sprintf "%d:%d: %s" loc.Location.line loc.Location.col msg))
    | Obuild_parser.Parser_error (loc, msg) ->
        raise
          (Project.InvalidConfFile
             (Printf.sprintf "%d:%d: %s" loc.Location.line loc.Location.col msg))
  in
  (* Apply ocaml_extra_args side effect *)
  (match proj.Project.ocaml_extra_args with
  | Some args -> Gconf.gconf.Gconf.ocaml_extra_args <- args
  | None -> ());
  (* Validate file existence *)
  Project.check proj;
  proj
