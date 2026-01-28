(** New parser integration for Project reading

    This module provides an alternative to Project.read that uses the new parser (obuild_lexer ->
    obuild_parser -> obuild_validate).

    It's in a separate module to avoid cyclic dependencies between Project and Obuild_validate. *)

open Filepath

(** Convert Project.Generator.t to Generators.custom *)
let convert_generator_to_custom (gen : Project.Generator.t) : Generators.custom =
  let custom_match = match gen.Project.Generator.match_type with
    | Project.Generator.Match_suffix s -> Generators.Match_suffix s
    | Project.Generator.Match_filename s -> Generators.Match_filename s
    | Project.Generator.Match_pattern s -> Generators.Match_pattern s
    | Project.Generator.Match_directory -> Generators.Match_directory
  in
  {
    Generators.custom_name = gen.Project.Generator.name;
    custom_match;
    custom_command = gen.Project.Generator.command;
    custom_outputs = gen.Project.Generator.outputs;
    custom_module_name = gen.Project.Generator.module_name;
    custom_multi_input = gen.Project.Generator.multi_input;
  }

(** Register custom generators from project *)
let register_generators proj =
  (* Clear any previously registered custom generators *)
  Generators.clear_custom_generators ();
  (* Register new ones *)
  List.iter (fun gen ->
    Generators.register_custom (convert_generator_to_custom gen)
  ) proj.Project.generators

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
  (* Register custom generators *)
  register_generators proj;
  (* Validate file existence *)
  Project.check proj;
  proj
