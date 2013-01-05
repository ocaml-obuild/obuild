open Ext
open Helper
open Types
open Printf
open Filepath

exception BuildDepAnalyzeFailed of string

exception DependencyMissing of string
exception DependencyFailedParsing of string

type module_desc =
    { module_use_threads : bool
    ; module_src_mtime : float
    ; module_has_interface : bool
    ; module_intf_mtime : float
    ; dep_cwd_modules : modname list
    ; dep_other_modules : modname list
    }

type dep_opt =
    { dep_withopt : bool
    ; dep_src_dir : string option
    }

(* return the (modules list) dependency for a specific file *)
let runOcamldep gconf dopt modname =
    let srcFile = with_optpath dopt.dep_src_dir (filename_of_module modname) in
    let args = ["ocamldep" ^ (if dopt.dep_withopt then ".opt" else "")]
             @ (maybe [] (fun x -> ["-I"; x]) dopt.dep_src_dir)
             @ ["-modules"; filepath_to_string srcFile ] in
    match run_with_outputs gconf args with
    | Success (f,_) -> (
        match Utils.toKV f with
        | (_,None)   -> raise (BuildDepAnalyzeFailed ("assumption failed: " ^ f))
        | (_,Some v) -> let vStripped = string_stripSpaces v in
                        if vStripped = ""
                               then []
                               else List.map wrap_module (List.map string_stripSpaces (string_words vStripped))
        )
    | Failure er -> raise (BuildDepAnalyzeFailed er)

(* differentiate if the dependency is system or is internal to the project *)
type dep_type = System | Internal

type data_dep =
    { dep_meta : Meta.meta option
    ; dep_type : dep_type
    }

type build_opts =
    { opt_nb_jobs_par : int
    ; opt_dump_dot    : bool
    }

let defaultBuildOpts =
    { opt_nb_jobs_par = 1
    ; opt_dump_dot    = false
    }

type common_opt =
    { common_build_native : bool (* build native or bytecode *)
    ; common_use_withopt : bool (* use the .opt compiler *)
    }

type data_cache =
    { data_deps        : (dep_main_name, data_dep) Hashtbl.t
    ; data_file_mtime  : (filepath, float) Hashtbl.t
    ; data_ocamlcfg    : (string, string) Hashtbl.t
    ; data_gconf       : general_conf
    ; data_projFile    : obuild
    ; data_commonopts  : common_opt
    ; data_buildopts   : build_opts
    }

(* get all the dependencies required
 * and prepare the global cache of value *)
let prepare gconf projFile buildOpts =
    let ocamlCfg = Prog.getOcamlConfig gconf true in
    let stdlibPath =
        try List.assoc "standard_library" ocamlCfg
        with Not_found -> failwith "standard library configuration is missing"
        in

    let depsTable = Hashtbl.create 16 in
    let internalLibs = List.map (fun lib -> lib.lib_name) projFile.obuild_libs in
    let getDepMeta dep =
        verbose gconf Debug "get dependency META %s\n%!" dep.dep_name;
        let metaFile = wrap_filepath (stdlibPath </> dep.dep_name </> "META") in
        (if not (Filesystem.exists metaFile)
            then raise (DependencyMissing dep.dep_name)
        );

        try Meta.parseFile metaFile
        with e -> raise (DependencyFailedParsing (Printexc.to_string e))
        in
    let getDep dep =
        if not (Hashtbl.mem depsTable dep.dep_name) then (
            let dataDep =
                if List.mem dep.dep_name internalLibs then (
                    { dep_meta = None; dep_type = Internal }
                ) else (
                    { dep_meta = Some (getDepMeta dep); dep_type = System }
                ) in
            Hashtbl.add depsTable dep.dep_name dataDep
        )
        in
    let getDeps deps = List.iter (fun (dep,_) -> getDep dep) deps in

    List.iter (fun lib -> getDeps lib.lib_builddeps) projFile.obuild_libs;
    List.iter (fun exe -> getDeps exe.exe_builddeps) projFile.obuild_exes;

    List.iter (fun v -> 
        match v.dep_meta with
        | None      -> ()
        | Some meta -> 
                        Meta.iterate (fun pkg ->
                            List.iter (fun req -> getDep req) pkg.Meta.package_requires
                       ) meta
    ) (Hashtbl.fold (fun _ v acc -> v :: acc) depsTable []);

    { data_deps        = depsTable
    ; data_file_mtime  = Hashtbl.create 64
    ; data_ocamlcfg    = hashtbl_fromList ocamlCfg
    ; data_projFile    = projFile
    ; data_gconf       = gconf
    ; data_buildopts   = buildOpts
    ; data_commonopts  =
            { common_build_native = true (* hardcoded for now, till we got bytecode support *)
            ; common_use_withopt  = gconf.conf_withopt
            }
    }

type compile_type = CompileModule    of modname
                  | CompileInterface of modname
                  | CompileC         of filename

type compilation_state =
    { compilation_modules  : (modname, module_desc) Hashtbl.t
    ; compilation_csources : filename list
    ; compilation_dag      : compile_type Dag.t
    ; compilation_order    : modname list list
    ; compilation_builddir : filepath
    ; compilation_paths    : filepath list
    ; mutable compilation_runqueue : unit list (* NOT_IMPLEMENTED *)
    }

(* prepare modules dependencies and various compilation state
 * that is going to be required for compilation and linking.
 *)
let prepare_compilation cache buildDir target toplevelModules =
    let modulesDeps = Hashtbl.create 64 in
    let rec loop modname =
        if Hashtbl.mem modulesDeps modname
        then ()
        else (
            verbose cache.data_gconf Verbose "Analysing %s\n%!" (modname.modname);
            let srcFile = with_optpath target.target_srcdir (filename_of_module modname) in
            let intfFile = with_optpath target.target_srcdir (interface_of_module modname) in
            let modTime = Filesystem.getModificationTime srcFile in
            let hasInterface = Filesystem.exists intfFile in
            let intfModTime = Filesystem.getModificationTime intfFile in

            verbose cache.data_gconf Debug "  %s has mtime %f\n%!" (modname.modname) modTime;
            if hasInterface then
                verbose cache.data_gconf Debug "  %s has interface (mtime=%f)\n%!" modname.modname intfModTime;

            let dopt = { dep_withopt = true
                       ; dep_src_dir = target.target_srcdir
                       } in
            let allDeps = runOcamldep cache.data_gconf dopt modname in
            verbose cache.data_gconf Debug "  %s depends on %s\n%!" (modname.modname) (String.concat "," (List.map modname_to_string allDeps));
            let (cwdDeps, otherDeps) = List.partition (fun dep ->
                let filename = with_optpath target.target_srcdir (filename_of_module dep) in
                Filesystem.exists filename
                ) allDeps in
            (*List.iter (fun cwdDep -> Dag.add_edge modname cwdDep dag) cwdDeps;*)
            let use_threads = List.mem (wrap_module "Thread") otherDeps in
            Hashtbl.add modulesDeps modname { module_use_threads   = use_threads
                                            ; module_has_interface = hasInterface
                                            ; module_src_mtime     = modTime
                                            ; module_intf_mtime    = intfModTime
                                            ; dep_cwd_modules      = cwdDeps
                                            ; dep_other_modules    = otherDeps
                                            };
            List.iter loop cwdDeps
        )
        in
    let get_dag () =
        let dag = Dag.init () in
        let h = hashtbl_map (fun dep -> dep.dep_cwd_modules) modulesDeps in
        while Hashtbl.length h > 0 do
            let freeModules = Hashtbl.fold (fun k v acc -> if v = [] then k :: acc else acc) h [] in
            if freeModules = []
                then failwith "internal error in dependencies"
                else ();
            List.iter (fun m ->
                Hashtbl.iter (fun k v ->
                    if k <> m then (
                        if List.mem m v then (
                            let kdep = Hashtbl.find modulesDeps k in
                            if kdep.module_has_interface
                                then (
                                    Dag.addEdge (CompileInterface k) (CompileModule m) dag;
                                    Dag.addEdge (CompileModule k) (CompileInterface k) dag
                                ) else
                                    Dag.addEdge (CompileModule k) (CompileModule m) dag
                        )
                    )
                ) h;

                (* make sure if the module doesn't have any parents
                 * that we don't forget about the node *)
                let mdep = Hashtbl.find modulesDeps m in
                if mdep.module_has_interface
                    then (
                        if not (Dag.existsNode (CompileInterface m) dag)
                            then (Dag.addNode (CompileInterface m) dag;
                                  Dag.addEdge (CompileModule m) (CompileInterface m) dag
                            )
                    ) else (
                        if not (Dag.existsNode (CompileModule m) dag)
                            then Dag.addNode (CompileModule m) dag
                    )
            ) freeModules;

            hashtbl_modify_all (fun v -> List.filter (fun x -> not (List.mem x freeModules)) v) h;
            List.iter (Hashtbl.remove h) freeModules;
        done;
        dag
        in
    (* create a very simplified DAG using a list.
     * it doesn't capture fine grained dependencies,
     * so it can't parallelize the best way, however there's still some
     * room for parallelization for blocks of free modules. *)
    let get_order () =
        let order = ref [] in
        (* copy and simplify the module deps hashtbl *)
        let h = hashtbl_map (fun dep -> dep.dep_cwd_modules) modulesDeps in
        while Hashtbl.length h > 0 do
            let freeModules = Hashtbl.fold (fun k v acc -> if v = [] then k :: acc else acc) h [] in
            if freeModules = []
                then failwith "internal error in dependencies"
                else ();
            hashtbl_modify_all (fun v -> List.filter (fun x -> not (List.mem x freeModules)) v) h;
            List.iter (Hashtbl.remove h) freeModules;
            order := freeModules :: !order
        done;
        List.rev !order
        in

    (* prepare the module hashtbl *)
    List.iter (fun m -> loop m) toplevelModules;

    (* we don't have a dependency tree for C sources files, so
     * for now we rely on the user to specify the compilation order
     * in the project file, and we serialize everything.
     *)
    let append_c_deps dag =
        let rec loop prev nodes =
            match nodes with
            | []             -> ()
            | current::[]    -> Dag.addEdge (CompileC current) (CompileC prev) dag
            | current::nexts -> Dag.addEdge (CompileC current) (CompileC prev) dag;
                                loop current nexts
            in
        match target.target_csources with
        | []    -> ()
        | x::[] -> Dag.addNode (CompileC x) dag
        | x::xs -> loop x xs
        in

    let dag = get_dag () in
    append_c_deps dag;

    if cache.data_buildopts.opt_dump_dot
        then (
            let dotDir = Dist.createBuildDest Dist.Dot in
            let path = wrap_filepath (dotDir.filepath </> (target.target_name ^ ".dot")) in
            let dotContent = Dag.toDot (fun x -> match x with
                                                 | CompileModule x    -> "mod " ^ x.modname
                                                 | CompileInterface x -> "intf " ^ x.modname
                                                 | CompileC x         -> "C " ^ x.filename
                                       ) target.target_name true dag in
            Filesystem.writeFile path dotContent;
        );

    { compilation_modules  = modulesDeps
    ; compilation_csources = target.target_csources
    ; compilation_order    = get_order ()
    ; compilation_dag      = dag
    ; compilation_builddir = buildDir
    ; compilation_paths    = [buildDir] @ (maybe [] (fun v -> [wrap_filepath v]) target.target_srcdir)
    ; compilation_runqueue = []
    }

