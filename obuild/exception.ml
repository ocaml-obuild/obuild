open Printf
open Helper
open Modname
open Ext.Filepath
open Types

(* TODO normalize exit code *)

(* automatic re-configuration trial *)

let already_retried_once = ref false

let retry_config_and_build () =
  if not !already_retried_once then (
    let previous_configure_command =
      try Utils.with_in_file (Project.findLastInvocationPath `Config) input_line
      with _no_such_file ->
        let default_config_cmd = "obuild configure" in
        Utils.string_list_to_file [default_config_cmd]
          (Project.findLastInvocationPath `Config);
        default_config_cmd
    in
    eprintf "trying to [re]configure\n\
      executing: %s\n%!" previous_configure_command;
    let config_exit_code = Sys.command previous_configure_command in
    if config_exit_code = 0 then (
      let build_command =
        try Utils.with_in_file (Project.findLastInvocationPath `Build) input_line
        with _no_such_file ->
          let default_build_cmd = "obuild build" in
          Utils.string_list_to_file [default_build_cmd]
            (Project.findLastInvocationPath `Build);
          default_build_cmd
      in
      eprintf "trying to build\n\
        executing: %s\n%!" build_command;
      let _build_exit_code = Sys.command build_command in ()
    ) else (
      eprintf "configure went bad: ret. code: %d\n%!" config_exit_code;
      exit config_exit_code
    )
  );
  already_retried_once := true

let show exn =
    let error fmt = eprintf ("%serror%s: " ^^ fmt) (color_white ()) (color_white ()) in
    match exn with
    | Arg.Bad err       -> eprintf "%s\n" err; exit 2
    | Arg.Help h        -> eprintf "%s\n" h; exit 0
    (* project file related *)
    | Project.NoConfFile        -> error "couldn't find obuild file\n"; exit 3
    | Project.MultipleConfFiles -> error "multiples obuild files found\n"; exit 3
    | Project.FileDoesntExist (t,f) ->
        error "project is referencing in %s, a file %s that cannot be found\n"
                (Target.get_target_name t) (fn_to_string f);
        exit 3
    | Project.ModuleDoesntExist (t,m) ->
        error "project is referencing in '%s', a module %s that cannot be found\n"
                (Target.get_target_name t) (Hier.hier_to_string m);
        exit 3
    | Project.ModuleListEmpty l ->
        error "library %s doesn't have any modules defined.\n" (lib_name_to_string l);
        exit 3
    | Project.InvalidConfFile c ->
        error "configuration file appears invalid: %s\n" c; exit 3
    | Project.BlockSectionAsValue s ->
        error "trying to define a section %s using parameter syntax:\n" s;
        eprintf "  spurious colon between section definition and section name\n";
        exit 3
    | Expr.CannotParseContraints (builddep, s) ->
        error "cannot parse contraints for build dependency '%s': %s\n" builddep s;
        exit 3
    (* dist directory related *)
    | Dist.NotADirectory -> error "dist is not a directory\n"; exit 4
    | Dist.DoesntExist   -> error "we need to configure first\n"; retry_config_and_build ()
    | Dist.MissingDestinationDirectory dir -> error "missing destination directory: %s\n" (Dist.buildtype_to_string dir); exit 4
    (* types stuff *)
    | Types.TargetNameNoType s      ->
        error "Unknown target '%s' with no prefix:\n" s;
        error "  targets need to start by one of lib-,exe-,bench-,test-,example-\n";
        exit 4
    | Types.TargetUnknownType (p,s) ->
        error "unknown type prefix '%s' in '%s':\n" p s;
        error "  targets need to start by one of lib-,exe-,bench-,test-,example-\n";
        exit 4
    | Types.TargetNotRecognized s   ->
        error "Unknown target specified '%s'\n" s;
        exit 4
    (* reconfigure *)
    | Configure.ConfigChanged r ->
            (match r with
            | "digest" -> error "project file changed.\n"; retry_config_and_build ()
            | _        -> error "config changed (reason=%s).\n" r; retry_config_and_build ()
            )
    | Configure.ConfigurationMissingKey k ->
        error "cannot find key %s in setup.\n" k; retry_config_and_build ()
    | Configure.ConfigurationTypeMismatch (k,t,v) ->
        error "%s type mismatch (got '%s') in setup key %s.\n" t v k; retry_config_and_build ()
    | Meta.MetaParseError (fp,err) ->
        error "unexpected parse error '%s' in meta file %s\n" err (fp_to_string fp); exit 4
    | Meta.ArchiveNotFound (path, dep, preds) ->
        error "archive %s not found in %s (%s)\n" (Utils.showList "," Meta.predicate_to_string preds) (lib_name_to_string dep) (fp_to_string path); exit 4
    | Analyze.SublibraryDoesntExists dep ->
        error "dependency %s not found\n" (lib_name_to_string dep); exit 4
    (* build related failure *)
    | Prepare.ModuleDependsItself m  -> error "cyclic dependency module detected in module %s\n" (Hier.hier_to_string m); exit 5
    | Prepare.ModuleNotFound (paths,m) ->
        error "module not found %s - search paths:\n" (Hier.hier_to_string m);
        List.iter (fun path -> eprintf "\t%s\n" (fp_to_string path)) paths;
        exit 5
    | Prepare.ModuleDependenciesProblem l ->
        error "cyclic dependency detected. cannot infer dependencies between modules:\n";
        eprintf "\t%s\n" (Utils.showList ", " Hier.hier_to_string l);
        exit 5
    | Build.CompilationFailed e       -> eprintf "\n%s\n%!" e; exit 6
    | Build.CCompilationFailed e      -> eprintf "\n%s\n%!" e; exit 6
    | Buildprogs.LinkingFailed e           -> eprintf "\n%s\n%!" e; exit 7
    | Dependencies.BuildDepAnalyzeFailed e -> eprintf "\n%s" e; exit 8
    | Dependencies.DependenciesMissing missing ->
        begin match List.length missing with
        | 0 -> assert false
        | 1 -> error "missing dependency '%s'\n" (List.hd missing); exit 9
        | _ -> eprintf "missing dependencies:\n%s\n" (Utils.showList "\n" (fun x -> x) missing); exit 9
        end
    (* others exception *)
    | Unix.Unix_error (err, fname, params) ->
        error "unexpected unix error: \"%s\" during %s(%s)\n" (Unix.error_message err) fname params;
        exit 20
    | Exit              -> ()
    | e                 -> eprintf "uncaught exception\n"; raise e
