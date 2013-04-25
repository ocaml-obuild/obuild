open Ext.Fugue
open Ext.Filepath
open Ext
open Helper
open Types
open Printf
open Gconf
open Target
open Dependencies
open Pp

exception SublibraryDoesntExists of dep_name
exception OcamlConfigMissing of string

(* differentiate if the dependency is system or is internal to the project *)
type dep_type = System | Internal

type dependency_tag = Target of name | Dependency of lib_name

type cpkg_config =
    { cpkg_conf_libs : string list
    ; cpkg_conf_includes : filepath list
    }

(* this is a read only config of the project for configuring and building.
 *)
type project_config =
    { project_dep_data    : (lib_name, dep_type) Hashtbl.t
    ; project_pkg_meta    : (dep_main_name, Meta.meta) Hashtbl.t
    ; project_pkgdeps_dag : dependency_tag Dag.t
    ; project_targets_dag : Types.name Dag.t
    ; project_all_deps    : dependency list
    ; project_file        : Project.obuild
    ; project_ocamlcfg    : (string, string) Hashtbl.t
    ; project_ocamlmkcfg  : (string, string) Hashtbl.t
    ; project_cpkgs       : (c_dep_name, cpkg_config) Hashtbl.t
    }

let get_meta_from_disk dep =
    verbose Debug "  fetching META %s\n%!" dep.lib_main_name;
    try Meta.findLib dep.lib_main_name
    with Meta.LibraryNotFound name -> raise (DependencyMissing name)

let get_meta_cache metaTable dep =
    try
        Hashtbl.find metaTable dep.lib_main_name
    with Not_found ->
        let r = get_meta_from_disk dep in
        Hashtbl.add metaTable dep.lib_main_name r;
        r

let get_ocaml_config_key_hashtbl key h =
    try Hashtbl.find h key
    with Not_found -> raise (OcamlConfigMissing key)

let get_ocaml_config_key key project = get_ocaml_config_key_hashtbl key project.project_ocamlcfg

let get_pkg_deps target project =
    let pkgs = Taskdep.linearize project.project_pkgdeps_dag Taskdep.FromParent [Target target.target_name] in
    List.rev (list_filter_map (fun pkg -> match pkg with Dependency d -> Some d | Target _ -> None) pkgs)

let get_c_pkg cname project =
    try Hashtbl.find project.project_cpkgs cname
    with Not_found -> failwith (sprintf "C package %s not found in the hashtbl: internal error" cname)

let is_pkg_internal project pkg = Hashtbl.find project.project_dep_data pkg = Internal
let is_pkg_system project pkg   = Hashtbl.find project.project_dep_data pkg = System

let get_pkg_meta lib project =
    try Hashtbl.find project.project_pkg_meta lib.lib_main_name
    with Not_found -> failwith (sprintf "package %s not found in the hashtbl: internal error" (lib_name_to_string lib))

let get_internal_library_deps project target =
    let internalDeps = Dag.getChildren project.project_targets_dag target.target_name in
    list_filter_map (fun name ->
        match name with
        | LibName lname -> Some lname
        | _             -> None
    ) internalDeps

(* all the standard libraries shipped with ocaml, comes *without* META files, so
 * we pre-populate the META cache with whatever we need by scanning the
 * directory that ocaml use as standard_library (found by running ocamlc -config).
 *
 * it allows to bootstrap better when ocamlfind has not been yet installed or
 * to detect difference of opinions of where the stdlib is, between ocamlfind and ocamlc.
 *)
let initializeSystemStdlib ocamlCfg metaTable =
    let (majorVer, minorVer, otherVer) = Prog.getOcamlVersion () in
    let stdlibPath = fp (get_ocaml_config_key_hashtbl "standard_library" ocamlCfg) in
    let stdlibLibs =
        Filesystem.list_dir_pred_map (fun n ->
            let ext = Filetype.get_extension n in
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
                  (if List.mem (fn libCmxa) stdlibLibs then [([Meta.Pred_Native], libCmxa)] else [])
                @ (if List.mem (fn libCma) stdlibLibs then [([Meta.Pred_Byte], libCma)] else [])
                in
            let meta = { (Meta.newPkg lib) with
                              Meta.package_directory = fp_to_string stdlibPath
                            ; Meta.package_requires  = [] (* AFAIK this is always empty for stdlibs *)
                            ; Meta.package_version   = sprintf "%d.%d.%s" majorVer minorVer otherVer
                            ; Meta.package_archives  = archives
                       } in
            Hashtbl.add metaTable lib (stdlibPath </> fn ("META-" ^ lib), meta)
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
let prepare projFile =
    verbose Verbose "analyzing project\n%!";
    let ocamlCfg = Prog.getOcamlConfig () in
    let ocamlMkCfg = readOcamlMkConfig (Hashtbl.find ocamlCfg "standard_library") in

    let depsTable  = Hashtbl.create 16 in
    let cpkgsTable = Hashtbl.create 1 in
    let depsDag    = Dag.init () in
    let targetsDag = Dag.init () in

    let missingDeps = ref StringSet.empty in

    let metaTable  = Hashtbl.create 8 in
    initializeSystemStdlib ocamlCfg metaTable;

    (* check for findlib / ocaml configuration mismatch *)
    let () =
        let stdlibPath = fp (get_ocaml_config_key_hashtbl "standard_library" ocamlCfg) in
        if not (List.exists (fun p -> string_startswith (fp_to_string p) (fp_to_string stdlibPath)) (FindlibConf.get_paths ())) then (
            Meta.meta_path_warning := true
        )
        in

    let allTargets = Project.get_all_buildable_targets projFile in

    let internalLibs = List.map (fun lib -> lib.Project.lib_name.lib_main_name) projFile.Project.libs in
    let isInternal lib = List.mem lib.lib_main_name internalLibs in

    (* establish inter-dependencies in the project.
     * only consider internal libraries *)
    List.iter (fun target ->
        Dag.addNode target.target_name targetsDag;
        List.iter (fun (dep, _) ->
            if isInternal dep then (
                verbose Debug "  internal depends: %s\n" (lib_name_to_string dep);
                Dag.addEdge target.target_name (LibName dep) targetsDag;
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
                let iLibDep = Dependency iLib.Project.lib_name in
                Dag.addNode iLibDep depsDag;
                List.iter (fun (reqDep,_) ->
                    verbose Debug "  library %s depends on %s\n" (lib_name_to_string iLib.Project.lib_name) (lib_name_to_string reqDep);
                    Dag.addEdge iLibDep (Dependency reqDep) depsDag;
                    loop reqDep
                ) iLib.Project.lib_target.target_obits.target_builddeps;
                Internal
            ) else (
                try begin
                  let (_, meta) = get_meta_cache metaTable dep in
                  Dag.addNode (Dependency dep) depsDag;
                  let pkg =
                      try Meta.find dep.lib_subnames meta
                      with Not_found -> raise (SublibraryDoesntExists dep)
                      in
                  List.iter (fun (preds, reqDeps) ->
                      match preds with
                      | Some [Meta.Pred_Toploop] -> ()
                      | _ ->
                          List.iter (fun reqDep ->
                              verbose Debug "  library %s depends on %s\n" (lib_name_to_string dep) (lib_name_to_string reqDep);
                              Dag.addEdge (Dependency dep) (Dependency reqDep) depsDag;
                              loop reqDep
                          ) reqDeps
                  ) pkg.Meta.package_requires;
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
            | LibName l -> Dag.addNode (Dependency l) depsDag; Dag.addEdge (Dependency l)
            | _         -> fun _ _ -> ()
            )
            in
        List.iter (fun (dep,constr) ->
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

    if gconf.conf_dump_dot
        then (
            let dotDir = Dist.createBuildDest Dist.Dot in
            let path = dotDir </> fn "dependencies.dot" in
            let toString t = match t with
                             | Target s     -> "target(" ^ name_to_string s ^ ")"
                             | Dependency s -> lib_name_to_string s
                             in
            let dotContent = Dag.toDot toString "dependencies" true depsDag in
            Filesystem.writeFile path dotContent;

            let ipath = dotDir </> fn "internal-dependencies.dot" in
            let dotIContent = Dag.toDot name_to_string "internal-dependencies" true targetsDag in
            Filesystem.writeFile ipath dotIContent;
        );

    { project_dep_data    = depsTable
    ; project_pkgdeps_dag = depsDag
    ; project_pkg_meta    = metaTable
    ; project_targets_dag = targetsDag
    ; project_all_deps    = List.concat $ List.map (fun target -> target.target_obits.target_builddeps) allTargets
    ; project_ocamlcfg    = ocamlCfg
    ; project_ocamlmkcfg  = ocamlMkCfg
    ; project_file        = projFile
    ; project_cpkgs       = cpkgsTable
    }
