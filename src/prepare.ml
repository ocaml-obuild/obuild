open Ext
open Helper
open Types
open Printf
open Filepath
open Gconf
open Target

exception BuildDepAnalyzeFailed of string
exception BuildCDepAnalyzeFailed of string

exception DependencyMissing of string
exception DependencyFailedParsing of string

exception Sublibrary_doesnt_exists of dep_name

(* differentiate if the dependency is system or is internal to the project *)
type dep_type = System of Meta.meta | Internal

type build_opts =
    { opt_nb_jobs_par : int
    ; opt_dump_dot    : bool
    }

let defaultBuildOpts =
    { opt_nb_jobs_par = 1
    ; opt_dump_dot    = false
    }

type common_opts =
    { common_build_native : bool (* build native or bytecode *)
    ; common_use_withopt  : bool (* use the .opt compiler *)
    }

type dependency_tag = Target of string | Dependency of dep_name

(* this is a real only config of the project for configuring and building.
 *)
type project_config =
    { project_dep_data   : (dep_main_name, dep_type) Hashtbl.t
    ; project_deps_dag   : dependency_tag Dag.t
    ; project_all_deps   : dependency list
    ; project_file       : Project.obuild
    ; project_ocamlcfg   : (string, string) Hashtbl.t
    ; project_commonopts : common_opts
    ; project_buildopts  : build_opts
    }

let get_meta stdlibPath dep =
    let metaFile = (stdlibPath </> fn dep.dep_name) </> fn "META" in
    (if not (Filesystem.exists metaFile)
        then raise (DependencyMissing dep.dep_name)
    );

    try Meta.parseFile metaFile
    with e -> raise (DependencyFailedParsing (Printexc.to_string e))

(* get all the dependencies required
 * and prepare the global bstate.of value *)
let prepare projFile buildOpts =
    let ocamlCfg = Prog.getOcamlConfig () in
    let stdlibPath =
        try fp (List.assoc "standard_library" ocamlCfg)
        with Not_found -> failwith "standard library configuration is missing"
        in

    let depsTable = Hashtbl.create 16 in
    let internalLibs = List.map (fun lib -> Project.dep_name_of_string lib.Project.lib_name) projFile.Project.libs in

    let depsDag = Dag.init () in
    let allTargets = List.map Project.lib_to_target projFile.Project.libs
                   @ List.map Project.exe_to_target projFile.Project.exes
                   @ List.map Project.test_to_target projFile.Project.tests
                   in
    let rec loop dep =
        let isInternal = List.mem dep internalLibs in
        let dataDep =
            if isInternal then (
                Internal
            ) else (
                verbose Debug "get dependency META %s\n%!" dep.dep_name;
                let meta = get_meta stdlibPath dep in
                Dag.addNode (Dependency dep) depsDag;
                let pkg =
                    try Meta.find dep.dep_subname meta
                    with Not_found -> raise (Sublibrary_doesnt_exists dep)
                    in
                List.iter (fun reqDep ->
                    Dag.addEdge (Dependency dep) (Dependency reqDep) depsDag;
                    loop reqDep
                ) pkg.Meta.package_requires;

                System meta
            )
            in
        if not (Hashtbl.mem depsTable dep.dep_name) then (
            Hashtbl.add depsTable dep.dep_name dataDep
        );
        ()
        in
    List.iter (fun target ->
        let nodeTarget = Target target.target_name in
        Dag.addNode nodeTarget depsDag;
        List.iter (fun (dep,constr) ->
            Dag.addEdge nodeTarget (Dependency dep) depsDag;
            loop dep;
        ) target.target_builddeps;
    ) allTargets;

    if buildOpts.opt_dump_dot
        then (
            let dotDir = Dist.createBuildDest Dist.Dot in
            let path = dotDir </> fn "dependencies.dot" in
            let toString t = match t with
                             | Target s     -> "#" ^ s ^ "#"
                             | Dependency s -> Project.dep_name_to_string s
                             in
            let dotContent = Dag.toDot toString "dependencies" true depsDag in
            Filesystem.writeFile path dotContent;
        );

    { project_dep_data    = depsTable
    ; project_deps_dag    = depsDag
    ; project_all_deps    = List.concat $ List.map (fun target -> target.target_builddeps) allTargets
    ; project_ocamlcfg    = hashtbl_fromList ocamlCfg
    ; project_file        = projFile
    ; project_buildopts   = buildOpts
    ; project_commonopts  =
            { common_build_native = true (* hardcoded for now, till we got bytecode supported *)
            ; common_use_withopt  = gconf.conf_withopt
            }
    }
