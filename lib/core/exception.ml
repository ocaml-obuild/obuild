open Printf
open Helper
open Filepath

(* Exit codes used by obuild *)
let exit_arg_error        = 2   (* bad command-line argument *)
let exit_project_error    = 3   (* project file / configuration problem *)
let exit_dist_error       = 4   (* dist directory / reconfigure needed *)
let exit_dependency_error = 5   (* module-level dependency problem *)
let exit_compile_error    = 6   (* OCaml compilation failure *)
let exit_link_error       = 7   (* linking failure *)
let exit_dep_analyze      = 8   (* ocamldep / dependency analysis failure *)
let exit_missing_dep      = 9   (* missing library dependency *)
let exit_unix_error       = 20  (* unexpected Unix error *)
let exit_invalid_path     = 30  (* invalid filename / filepath *)
let exit_file_not_found   = 40  (* file not found in search paths *)

let show exn =
  let error fmt = eprintf ("%serror%s: " ^^ fmt) (color_white ()) (color_white ()) in
  match exn with
  | Arg.Bad err ->
      eprintf "%s\n" err;
      exit exit_arg_error
  | Arg.Help h ->
      eprintf "%s\n" h;
      exit 0
  (* project file related *)
  | Project.NoConfFile ->
      error "couldn't find obuild file\n";
      exit exit_project_error
  | Project.MultipleConfFiles ->
      error "multiples obuild files found\n";
      exit exit_project_error
  | Project.FileNotFound (t, f) ->
      error "project is referencing in %s, a file %s that cannot be found\n"
        (Target.get_target_name t) (fn_to_string f);
      exit exit_project_error
  | Project.ModuleNotFound (t, m) ->
      error "project is referencing in '%s', a module %s that cannot be found\n"
        (Target.get_target_name t) (Hier.to_string m);
      exit exit_project_error
  | Project.ModuleListEmpty l ->
      error "library %s doesn't have any modules defined.\n" (Libname.to_string l);
      exit exit_project_error
  | Project.InvalidConfFile c ->
      error "configuration file appears invalid: %s\n" c;
      exit exit_project_error
  | Project.BlockSectionAsValue s ->
      error "trying to define a section %s using parameter syntax:\n" s;
      eprintf "  spurious colon between section definition and section name\n";
      exit exit_project_error
  | Project.BadOcamlVersion (ver, c) ->
      error "wrong ocaml version: actual %s expected %s\n" ver (Expr.to_string c);
      exit exit_project_error
  | Expr.CannotParseConstraints (builddep, s) ->
      error "cannot parse constraints for build dependency '%s': %s\n" builddep s;
      exit exit_project_error
  (* dist directory related *)
  | Dist.DistNotADirectory ->
      error "dist is not a directory\n";
      exit exit_dist_error
  | Dist.DistNotFound ->
      error "run 'obuild configure' first\n";
      exit exit_dist_error
  | Dist.MissingDestinationDirectory dir ->
      error "missing destination directory: %s\n" (Dist.to_string dir);
      exit exit_dist_error
  (* types stuff *)
  | Target.TargetNameNoType s ->
      error "Unknown target '%s' with no prefix:\n" s;
      error "  targets need to start by one of lib-,exe-,bench-,test-,example-\n";
      exit exit_dist_error
  | Target.TargetUnknownType (p, s) ->
      error "unknown type prefix '%s' in '%s':\n" p s;
      error "  targets need to start by one of lib-,exe-,bench-,test-,example-\n";
      exit exit_dist_error
  | Target.TargetNotRecognized s ->
      error "Unknown target specified '%s'\n" s;
      exit exit_dist_error
  (* reconfigure *)
  | Configure.ConfigChanged r -> (
      match r with
      | "digest" ->
          error "project file changed. run 'obuild configure' again\n";
          exit exit_dist_error
      | _ ->
          error "config changed (reason=%s). run 'obuild configure' again\n" r;
          exit exit_dist_error)
  | Configure.ConfigurationMissingKey k ->
      error "cannot find key %s in setup. run 'obuild configure' again\n" k;
      exit exit_dist_error
  | Configure.ConfigurationTypeMismatch (k, t, v) ->
      error "%s type mismatch (got '%s') in setup key %s. run 'obuild configure' again\n" t v k;
      exit exit_dist_error
  | Meta.MetaParseError (fp, err) ->
      error "unexpected parse error '%s' in meta file %s\n" err (fp_to_string fp);
      exit exit_dist_error
  | Meta.ArchiveNotFound (path, dep, preds) ->
      error "archive %s not found in %s (%s)\n"
        (Utils.showList "," Meta.Predicate.to_string preds)
        (Libname.to_string dep) (fp_to_string path);
      exit exit_dist_error
  | Analyze.SublibraryNotFound dep ->
      error "dependency %s not found\n" (Libname.to_string dep);
      exit exit_dist_error
  (* build related failure *)
  | Prepare.Module.DependsItself m ->
      error "cyclic dependency module detected in module %s\n" (Hier.to_string m);
      exit exit_dependency_error
  | Prepare.Module.NotFound (paths, m) ->
      error "module not found %s - search paths:\n" (Hier.to_string m);
      List.iter (fun path -> eprintf "\t%s\n" (fp_to_string path)) paths;
      exit exit_dependency_error
  | Prepare.Module.DependenciesProblem l ->
      error "cyclic dependency detected. cannot infer dependencies between modules:\n";
      eprintf "\t%s\n" (Utils.showList ", " Hier.to_string l);
      exit exit_dependency_error
  | Build.CompilationFailed e ->
      eprintf "\n%s\n%!" e;
      exit exit_compile_error
  | Build.CCompilationFailed e ->
      eprintf "\n%s\n%!" e;
      exit exit_compile_error
  | Buildprogs.LinkingFailed e ->
      eprintf "\n%s\n%!" e;
      exit exit_link_error
  | Dependencies.BuildDepAnalyzeFailed e ->
      eprintf "\n%s\n%!" e;
      exit exit_dep_analyze
  | Dependencies.DependenciesMissing missing -> (
      match missing with
      | [] -> assert false
      | [dep] ->
          error "missing dependency '%s'\n" dep;
          exit exit_missing_dep
      | _ ->
          eprintf "missing dependencies:\n%s\n" (Utils.showList "\n" (fun x -> x) missing);
          exit exit_missing_dep)
  (* others exception *)
  | Unix.Unix_error (err, fname, params) ->
      error "unexpected unix error: \"%s\" during %s(%s)\n" (Unix.error_message err) fname params;
      exit exit_unix_error
  | Filepath.InvalidFilename f ->
      error "the filename \"%s\" is not valid, it contains a directory separator\n" f;
      exit exit_invalid_path
  | Utils.FileNotFoundInPaths (ds, f) ->
      error "File %s not found in directories %s\n" (fn_to_string f)
        (Utils.showList "; " fp_to_string ds);
      exit exit_file_not_found
  | Exit -> ()
  | e ->
      eprintf "uncaught exception\n";
      raise e
