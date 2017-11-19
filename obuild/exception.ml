open Printf
open Helper
open Ext.Filepath

(* TODO normalize exit code *)

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
                (Target.get_target_name t) (Hier.to_string m);
        exit 3
    | Project.ModuleListEmpty l ->
        error "library %s doesn't have any modules defined.\n" (Libname.to_string l);
        exit 3
    | Project.InvalidConfFile c ->
        error "configuration file appears invalid: %s\n" c; exit 3
    | Project.BlockSectionAsValue s ->
        error "trying to define a section %s using parameter syntax:\n" s;
        eprintf "  spurious colon between section definition and section name\n";
        exit 3
    | Project.BadOcamlVersion (ver,c) ->
      error "wrong ocaml version: actual %s expected %s\n" ver (Expr.to_string c);
      exit 3
    | Expr.CannotParseConstraints (builddep, s) ->
        error "cannot parse constraints for build dependency '%s': %s\n" builddep s;
        exit 3
    (* dist directory related *)
    | Dist.NotADirectory -> error "dist is not a directory\n"; exit 4
    | Dist.DoesntExist   -> error "run 'obuild configure' first\n"; exit 4
    | Dist.MissingDestinationDirectory dir -> error "missing destination directory: %s\n" (Dist.to_string dir); exit 4
    (* types stuff *)
    | Target.TargetNameNoType s      ->
        error "Unknown target '%s' with no prefix:\n" s;
        error "  targets need to start by one of lib-,exe-,bench-,test-,example-\n";
        exit 4
    | Target.TargetUnknownType (p,s) ->
        error "unknown type prefix '%s' in '%s':\n" p s;
        error "  targets need to start by one of lib-,exe-,bench-,test-,example-\n";
        exit 4
    | Target.TargetNotRecognized s   ->
        error "Unknown target specified '%s'\n" s;
        exit 4
    (* reconfigure *)
    | Configure.ConfigChanged r ->
            (match r with
            | "digest" -> error "project file changed. run 'obuild configure' again\n"; exit 4
            | _        -> error "config changed (reason=%s). run 'obuild configure' again\n" r; exit 4
            )
    | Configure.ConfigurationMissingKey k ->
        error "cannot find key %s in setup. run 'obuild configure' again\n" k; exit 4
    | Configure.ConfigurationTypeMismatch (k,t,v) ->
        error "%s type mismatch (got '%s') in setup key %s. run 'obuild configure' again\n" t v k; exit 4
    | Meta.MetaParseError (fp,err) ->
        error "unexpected parse error '%s' in meta file %s\n" err (fp_to_string fp); exit 4
    | Meta.ArchiveNotFound (path, dep, preds) ->
        error "archive %s not found in %s (%s)\n" (Utils.showList "," Meta.Predicate.to_string preds) (Libname.to_string dep) (fp_to_string path); exit 4
    | Analyze.SublibraryDoesntExists dep ->
        error "dependency %s not found\n" (Libname.to_string dep); exit 4
    (* build related failure *)
    | Prepare.Module.DependsItself m  -> error "cyclic dependency module detected in module %s\n" (Hier.to_string m); exit 5
    | Prepare.Module.NotFound (paths,m) ->
        error "module not found %s - search paths:\n" (Hier.to_string m);
        List.iter (fun path -> eprintf "\t%s\n" (fp_to_string path)) paths;
        exit 5
    | Prepare.Module.DependenciesProblem l ->
        error "cyclic dependency detected. cannot infer dependencies between modules:\n";
        eprintf "\t%s\n" (Utils.showList ", " Hier.to_string l);
        exit 5
    | Build.CompilationFailed e       -> eprintf "\n%s\n%!" e; exit 6
    | Build.CCompilationFailed e      -> eprintf "\n%s\n%!" e; exit 6
    | Buildprogs.LinkingFailed e           -> eprintf "\n%s\n%!" e; exit 7
    | Dependencies.BuildDepAnalyzeFailed e -> eprintf "\n%s\n%!" e; exit 8
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
    | Ext.Filepath.InvalidFilename f ->
        error "the filename \"%s\" is not valid, it contains a directory separator\n" f;
        exit 30
    | Utils.FileNotFoundInPaths (ds,f) ->
      error "File %s not found in directories %s\n" (fn_to_string f)
        (Utils.showList "; " fp_to_string ds);
      exit 40
    | Exit              -> ()
    | e                 -> eprintf "uncaught exception\n"; raise e
