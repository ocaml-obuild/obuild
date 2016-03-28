open Ext.Fugue
open Ext.Filepath
open Ext
open Helper
open Printf
open Gconf
open Target
open Dependencies

exception SublibraryDoesntExists of Libname.t
exception OcamlConfigMissing of string

(* differentiate if the dependency is system or is internal to the project *)
type dep_type = System | Internal

type dependency_tag = Target of Name.t | Dependency of Libname.t

type cpkg_config =
    { cpkg_conf_libs : string list
    ; cpkg_conf_includes : filepath list
    }

(* this is a read only config of the project for configuring and building.
 *)
type project_config =
    { project_dep_data    : (Libname.t, dep_type) Hashtbl.t
    ; project_pkgdeps_dag : dependency_tag Dag.t
    ; project_targets_dag : Name.t Dag.t
    ; project_all_deps    : dependency list
    ; project_file        : Project.t
    ; project_ocamlcfg    : (string, string) Hashtbl.t
    ; project_ocamlmkcfg  : (string, string) Hashtbl.t
    ; project_cpkgs       : (string, cpkg_config) Hashtbl.t
    }

let get_ocaml_config_key_hashtbl key h =
    try Hashtbl.find h key
    with Not_found -> raise (OcamlConfigMissing key)

let getOcamlConfigKey key =
  get_ocaml_config_key_hashtbl key (Prog.getOcamlConfig ())

let get_ocaml_config_key key project = get_ocaml_config_key_hashtbl key project.project_ocamlcfg

let get_pkg_deps target project =
    let pkgs = Taskdep.linearize project.project_pkgdeps_dag Taskdep.FromParent [Target target.target_name] in
    List.rev (list_filter_map (fun pkg -> match pkg with Dependency d -> Some d | Target _ -> None) pkgs)

let get_c_pkg cname project =
    try Hashtbl.find project.project_cpkgs cname
    with Not_found -> failwith (sprintf "C package %s not found in the hashtbl: internal error" cname)

let is_pkg_internal project pkg = Hashtbl.find project.project_dep_data pkg = Internal
let is_pkg_system project pkg   = Hashtbl.find project.project_dep_data pkg = System

let get_internal_library_deps project target =
    let internalDeps = Dag.getChildren project.project_targets_dag target.target_name in
    list_filter_map (fun name ->
        match name with
        | Name.Lib lname -> Some lname
        | _             -> None
    ) internalDeps

(* all the standard libraries shipped with ocaml, comes *without* META files, so
 * we pre-populate the META cache with whatever we need by scanning the
 * directory that ocaml use as standard_library (found by running ocamlc -config).
 *
 * it allows to bootstrap better when ocamlfind has not been yet installed or
 * to detect difference of opinions of where the stdlib is, between ocamlfind and ocamlc.
 *)
let initializeSystemStdlib ocamlCfg =
    let ocaml_ver = Hashtbl.find (Prog.getOcamlConfig ()) "version" in
    let stdlibPath = fp (get_ocaml_config_key_hashtbl "standard_library" ocamlCfg) in
    let stdlibLibs =
        Filesystem.list_dir_pred_map (fun n ->
            let ext = Filetype.of_filename n in
            if ext = Filetype.FileCMXA || ext = Filetype.FileCMA
                then Some n
                else None
        ) stdlibPath
        in
    let libs = list_uniq $ List.map (fun f -> fn_to_string $ Filepath.chop_extension f) stdlibLibs in
    List.iter (fun lib ->
        (* skip .p library which are just variant of the no .p library *)
        if not (string_endswith ".p" lib) then (
            verbose Verbose "initializing standard library : package %s\n" lib;
            let libCmxa = lib ^ ".cmxa" in
            let libCma  = lib ^ ".cma" in
            let archives =
                  (if List.mem (fn libCmxa) stdlibLibs then [([Meta.Predicate.Native], libCmxa)] else [])
                @ (if List.mem (fn libCma) stdlibLibs then [([Meta.Predicate.Byte], libCma)] else [])
                in
            let meta = { (Meta.Pkg.make lib) with
                              Meta.Pkg.directory = fp_to_string stdlibPath
                            ; Meta.Pkg.requires  = [] (* AFAIK this is always empty for stdlibs *)
                            ; Meta.Pkg.version   = ocaml_ver
                            ; Meta.Pkg.archives  = archives
                       } in
            Metacache.add lib (stdlibPath </> fn ("META-" ^ lib), meta)
        )
    ) libs

let readOcamlMkConfig filename =
    let lines = Utils.read_file_with
        (function "" -> None | s when s.[0] = '#' -> None | s -> Some s) 
        (filename ^ "/Makefile.config") in
    let h = Hashtbl.create 32 in
    List.iter (fun l ->
        let (k,v) = Utils.toKVeq l in
        Hashtbl.add h (String.lowercase k) (default "" v)
    ) lines;
    h

(* get all the dependencies required
 * and prepare the global bstate.of value *)
let prepare projFile user_flags =
    verbose Verbose "analyzing project\n%!";
    let ocamlCfg = Prog.getOcamlConfig () in
    let ocamlMkCfg = readOcamlMkConfig (Hashtbl.find ocamlCfg "standard_library") in

    let depsTable  = Hashtbl.create 16 in
    let cpkgsTable = Hashtbl.create 1 in
    let depsDag    = Dag.init () in
    let targetsDag = Dag.init () in

    let missingDeps = ref StringSet.empty in

    initializeSystemStdlib ocamlCfg;

    (* check for findlib / ocaml configuration mismatch *)
    let () =
        let stdlibPath = fp (get_ocaml_config_key_hashtbl "standard_library" ocamlCfg) in
        if not (List.exists (fun p -> string_startswith (fp_to_string p) (fp_to_string stdlibPath)) (FindlibConf.get_paths ())) then (
            Meta.path_warning := true
        )
        in

    let allTargets = Project.get_all_buildable_targets projFile user_flags in

    let internalLibs = List.map (fun lib -> lib.Project.Library.name.Libname.main_name) projFile.Project.libs in
    let isInternal lib = List.mem lib.Libname.main_name internalLibs in

    (* establish inter-dependencies in the project.
     * only consider internal libraries *)
    List.iter (fun target ->
        Dag.addNode target.target_name targetsDag;
        List.iter (fun (dep, _) ->
            if isInternal dep then (
                verbose Debug "  internal depends: %s\n" (Libname.to_string dep);
                Dag.addEdge target.target_name (Name.Lib dep) targetsDag;
            )
        ) (Target.get_all_builddeps target);
    ) allTargets;

    let add_missing dep = missingDeps := StringSet.add dep (!missingDeps) in

    (* load every dependencies META files and at the same time generate the
     * graph of inter-dependencies.
     *
     * This recursively load all dependencies and dependencies's dependencies.
     *)
    let rec loop dep =
        let dataDep () =
            if isInternal dep then (
                let iLib = Project.find_lib projFile dep in
                let iLibDep = Dependency iLib.Project.Library.name in
                Dag.addNode iLibDep depsDag;
                List.iter (fun (reqDep,_) ->
                    verbose Debug "  library %s depends on %s\n" (Libname.to_string iLib.Project.Library.name) (Libname.to_string reqDep);
                    Dag.addEdge iLibDep (Dependency reqDep) depsDag;
                    loop reqDep
                ) iLib.Project.Library.target.target_obits.target_builddeps;
                Internal
            ) else (
                try begin
                  let (_, meta) = Metacache.get dep.Libname.main_name in
                  Dag.addNode (Dependency dep) depsDag;
                  let pkg =
                      try Meta.Pkg.find dep.Libname.subnames meta
                      with Not_found -> raise (SublibraryDoesntExists dep)
                         | Meta.SubpackageNotFound _ -> raise (SublibraryDoesntExists dep)
                      in
                  List.iter (fun (preds, reqDeps) ->
                      match preds with
                      | [Meta.Predicate.Toploop] -> ()
                      | _ ->
                          List.iter (fun reqDep ->
                              verbose Debug "  library %s depends on %s\n" (Libname.to_string dep) (Libname.to_string reqDep);
                              Dag.addEdge (Dependency dep) (Dependency reqDep) depsDag;
                              loop reqDep
                          ) reqDeps
                  ) pkg.Meta.Pkg.requires;
                  System
                end with DependencyMissing dep -> (add_missing dep; System)
            )
            in
        if not (Hashtbl.mem depsTable dep) then (
            Hashtbl.add depsTable dep (dataDep ())
        );
        ()
        in
    List.iter (fun target ->
        verbose Debug "  getting dependencies for target %s\n%!" (Target.get_target_name target);
        let nodeTarget = Target target.target_name in
        Dag.addNode nodeTarget depsDag;
        (* if a lib, then we insert ourself as dependency for executable or other library *)
        let insertEdgeForDependency =
          (match target.target_name with
           | Name.Lib l -> Dag.addNode (Dependency l) depsDag; Dag.addEdge (Dependency l)
           | _         -> fun _ _ -> ()
          )
        in
        List.iter (fun (dep,constr) ->
            maybe_unit (fun c ->
                let (_,pkg) = Metacache.get dep.Libname.main_name in
                if not (Expr.eval pkg.Meta.Pkg.version c) then
                  raise (Dependencies.BuildDepAnalyzeFailed
                           (Libname.to_string dep ^ " (" ^ pkg.Meta.Pkg.version ^
                            ") doesn't match the constraint " ^ (Expr.to_string c)))
              ) constr;
            Dag.addEdge nodeTarget (Dependency dep) depsDag;
            insertEdgeForDependency (Dependency dep) depsDag;
            loop dep;
        ) (Target.get_all_builddeps target);

        if not (StringSet.is_empty !missingDeps) then
          raise (DependenciesMissing (StringSet.to_list !missingDeps));

        List.iter (fun (cpkg, cconstr) ->
            let ver = Prog.runPkgConfigVersion cpkg in
            (* TODO compare the constraints *)
            ignore cconstr; ignore ver;
            let pkgIncludes = List.map fp (Prog.runPkgConfigIncludes cpkg) in
            let pkgLibs = Prog.runPkgConfigLibs cpkg in
            let pkgConf = { cpkg_conf_libs = pkgLibs; cpkg_conf_includes = pkgIncludes } in
            Hashtbl.add cpkgsTable cpkg pkgConf
        ) target.target_cbits.target_cpkgs
    ) allTargets;

    if gconf.dump_dot
        then (
            let dotDir = Dist.create_build Dist.Dot in
            let path = dotDir </> fn "dependencies.dot" in
            let toString t = match t with
                             | Target s     -> "target(" ^ Name.to_string s ^ ")"
                             | Dependency s -> Libname.to_string s
                             in
            let dotContent = Dag.toDot toString "dependencies" true depsDag in
            Filesystem.writeFile path dotContent;

            let ipath = dotDir </> fn "internal-dependencies.dot" in
            let dotIContent = Dag.toDot Name.to_string "internal-dependencies" true targetsDag in
            Filesystem.writeFile ipath dotIContent;
        );

    { project_dep_data    = depsTable
    ; project_pkgdeps_dag = depsDag
    ; project_targets_dag = targetsDag
    ; project_all_deps    = List.concat $ List.map (fun target -> target.target_obits.target_builddeps) allTargets
    ; project_ocamlcfg    = ocamlCfg
    ; project_ocamlmkcfg  = ocamlMkCfg
    ; project_file        = projFile
    ; project_cpkgs       = cpkgsTable
    }
