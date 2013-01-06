(*
 * gather dependencies in hashtable and DAGs
 * and create compilation state for one target
 *)
open Ext
open Prepare
open Types
open Filepath
open Filetype
open Helper
open Gconf
open Modname
open Target

exception ModuleDependsItself of modname

type module_desc =
    { module_use_threads : bool
    ; module_src_mtime : float
    ; module_has_interface : bool
    ; module_intf_mtime : float
    ; dep_cwd_modules : modname list
    ; dep_other_modules : modname list
    }

(* live for the whole duration of a building process
 * which include compilations and linkings
 *)
type build_state =
    { bstate_config      : project_config
    ; bstate_files_mtime : Tracker.mtime_tracker
    }

type compile_type = CompileModule    of modname
                  | CompileInterface of modname
                  | CompileC         of filename

(* represent a single compilation *)
type compilation_state =
    { compilation_modules  : (modname, module_desc) Hashtbl.t
    ; compilation_csources : filename list
    ; compilation_dag      : compile_type Dag.t
    ; compilation_filesdag : file_id Dag.t
    ; compilation_builddir : filepath
    ; compilation_paths    : filepath list
    }

let init project =
    { bstate_files_mtime = Tracker.mtimetracker_init ()
    ; bstate_config      = project
    }

type dep_opt =
    { dep_withopt : bool
    ; dep_src_dir : filepath option
    }

let parse_output_KsemiVs onNonKV mapFstTy mapSndTys out =
    List.map (fun (k, mv) ->
        match mv with
        | None   -> onNonKV k
        | Some v -> (mapFstTy k, List.map mapSndTys (string_words_noempty v))
    ) (List.map Utils.toKV (string_lines_noempty out))

(* return the (modules list) dependency for a specific file *)
let runOcamldep dopt modname =
    let srcFile = with_optpath dopt.dep_src_dir (filename_of_module modname) in
    let args = [Prog.getOcamlDep ()]
             @ (maybe [] (fun x -> ["-I"; fp_to_string x]) dopt.dep_src_dir)
             @ ["-modules"; fp_to_string srcFile ] in
    match Process.run_with_outputs args with
    | Process.Failure er -> raise (BuildDepAnalyzeFailed er)
    | Process.Success (out,_) ->
        List.map snd (parse_output_KsemiVs
            (fun _ -> raise (BuildDepAnalyzeFailed ("assumption failed: " ^ out)))
            fp wrap_module out
        )

(* TODO
 * gcc escape spaces in filename with a \, tweak strings_words_noempty
 * to take that in consideration.
 *)
let runCCdep srcDir files : (filename * filepath list) list =
    let args = [Prog.getCC (); "-MM"] @ List.map (fun fn -> fp_to_string (srcDir </> fn)) files in
    match Process.run_with_outputs args with
    | Process.Failure err     -> raise (BuildCDepAnalyzeFailed err)
    | Process.Success (out,_) ->
        parse_output_KsemiVs
            (fun _ -> raise (BuildCDepAnalyzeFailed "missing semicolon in gcc dependency output"))
            fn fp out

(* prepare modules dependencies and various compilation state
 * that is going to be required for compilation and linking.
 *)
let prepare_target bstate buildDir target toplevelModules =
    verbose Verbose "prepare compilation for %s\n%!" (Target.get_target_name target);
    let modulesDeps = Hashtbl.create 64 in
    let rec loop modname =
        if Hashtbl.mem modulesDeps modname
        then ()
        else (
            verbose Verbose "Analysing %s\n%!" (modname.modname);
            let srcFile = with_optpath target.target_srcdir (filename_of_module modname) in
            let intfFile = with_optpath target.target_srcdir (interface_of_module modname) in
            let modTime = Filesystem.getModificationTime srcFile in
            let hasInterface = Filesystem.exists intfFile in
            let intfModTime = Filesystem.getModificationTime intfFile in

            verbose Debug "  %s has mtime %f\n%!" (modname.modname) modTime;
            if hasInterface then
                verbose Debug "  %s has interface (mtime=%f)\n%!" modname.modname intfModTime;

            let dopt = { dep_withopt = true
                       ; dep_src_dir = target.target_srcdir
                       } in
            (* TODO remove hd and replace by proper case checking with proper exception *)
            let allDeps = List.hd (runOcamldep dopt modname) in
            verbose Debug "  %s depends on %s\n%!" (modname.modname) (String.concat "," (List.map modname_to_string allDeps));
            let (cwdDeps, otherDeps) = List.partition (fun dep ->
                let filename = with_optpath target.target_srcdir (filename_of_module dep) in
                Filesystem.exists filename
                ) allDeps in
            let use_threads = List.mem (wrap_module "Thread") otherDeps in
            (if List.mem modname cwdDeps then
                raise (ModuleDependsItself modname)
            );
            Hashtbl.add modulesDeps modname { module_use_threads   = use_threads
                                            ; module_has_interface = hasInterface
                                            ; module_src_mtime     = modTime
                                            ; module_intf_mtime    = intfModTime
                                            ; dep_cwd_modules      = cwdDeps
                                            ; dep_other_modules    = otherDeps
                                            };
            (* TODO: don't query single modules at time, where ocamldep supports M modules *)
            List.iter loop cwdDeps
        )
        in
    (* create 2 dags per target
     * - stepsDag is a DAG of all the tasks to achieve the target (compilation only, not linking yet)
     * - filesDag is a DAG of all the files dependencies
     *     TODO: retrospectively, it would be better to just compute the filesDag needed stuff on demand.
     *           fix later.
     **)
    let get_dags () =
        let filesDag = Dag.init () in
        let stepsDag = Dag.init () in
        let h = hashtbl_map (fun dep -> dep.dep_cwd_modules) modulesDeps in
        while Hashtbl.length h > 0 do
            let freeModules = Hashtbl.fold (fun k v acc -> if v = [] then k :: acc else acc) h [] in
            if freeModules = []
                then failwith "internal error in dependencies"
                else ();
            List.iter (fun m ->
                let mdep = Hashtbl.find modulesDeps m in

                let mlNode  = file_id (Filetype.FileML, with_optpath target.target_srcdir (filename_of_module m)) in
                let mliNode = file_id (Filetype.FileMLI, with_optpath target.target_srcdir (interface_of_module m)) in
                let cmiNode = file_id (Filetype.FileCMI, buildDir </> cmi_of_module m) in

                let cmxNode = file_id (Filetype.FileCMX, buildDir </> cmx_of_module false false m) in
                let cmoNode = file_id (Filetype.FileCMO, buildDir </> cmo_of_module false false m) in
                let oNode   = file_id (Filetype.FileO, buildDir </> o_of_module m) in

                Hashtbl.iter (fun k v ->
                    if k <> m then (
                        let kCmiNode = file_id (Filetype.FileCMI, buildDir </> cmi_of_module k) in
                        let kCmxNode = file_id (Filetype.FileCMX, buildDir </> cmx_of_module false false k) in
                        let kCmoNode = file_id (Filetype.FileCMO, buildDir </> cmo_of_module false false k) in

                        if List.mem m v then (
                            Dag.addEdges [ (kCmxNode, cmxNode)
                                         ; (kCmoNode, cmoNode)
                                         ; (kCmiNode, cmiNode)
                                         ] filesDag;
                            let kdep = Hashtbl.find modulesDeps k in
                            if kdep.module_has_interface
                                then (
                                    Dag.addEdgesConnected [CompileModule k; CompileInterface k; CompileModule m] stepsDag
                                ) else
                                    Dag.addEdge (CompileModule k) (CompileModule m) stepsDag
                        )
                    )
                ) h;

                (* make sure if the module doesn't have any parents
                 * that we don't forget about the node *)
                Dag.addNode (CompileModule m) stepsDag;

                if mdep.module_has_interface then (
                    Dag.addEdge (CompileModule m) (CompileInterface m) stepsDag;

                    Dag.addEdges [ (cmxNode, mlNode) ; (cmoNode, mlNode)
                                 ; (cmxNode, cmiNode); (cmoNode, cmiNode)
                                 ; (cmiNode, mliNode)
                                 ; (oNode, mlNode)
                                 ] filesDag
                ) else (
                    Dag.addEdges [ (cmxNode, mlNode)
                                 ; (cmoNode, mlNode)
                                 ; (cmiNode, mlNode)
                                 ; (oNode, mlNode)
                                 ] filesDag
                )
            ) freeModules;

            hashtbl_modify_all (fun v -> List.filter (fun x -> not (List.mem x freeModules)) v) h;
            List.iter (Hashtbl.remove h) freeModules;
        done;

        (* just append each C sources as single node in the stepsDag *)
        if target.target_csources <> [] then (
            let objDeps = runCCdep (default emptyDir target.target_cdir) target.target_csources in

            List.iter (fun cSource ->
                let (fps : filepath list) =
                    try List.assoc (Filetype.replace_extension cSource Filetype.FileO) objDeps
                    with _ -> failwith ("cannot find dependencies for " ^ fn_to_string cSource)
                    in
                let cFile = with_optpath (target.target_cdir) cSource in
                let hFiles = List.map (fun x -> { fdep_ty = Filetype.FileH; fdep_path = x })
                    (List.filter (fun x -> Filetype.get_extension_path x = Filetype.FileH) fps)
                        in
                let oFile = buildDir </> (cSource <.> "o") in
                let cNode = { fdep_ty = Filetype.FileC; fdep_path = cFile } in
                let oNode = { fdep_ty = Filetype.FileO; fdep_path = oFile } in

                (* add C source information into the files DAG *)
                Dag.addEdge oNode cNode filesDag;
                Dag.addChildrenEdges oNode hFiles filesDag;

                (* add C source compilation task into the step DAG *)
                Dag.addNode (CompileC cSource) stepsDag
            ) target.target_csources;
        );

        let allMods = hashtbl_keys modulesDeps in
        let allCmx = List.map (fun m -> file_id (Filetype.FileCMX, buildDir </> cmx_of_module false false m)) allMods in
        let allCmo = List.map (fun m -> file_id (Filetype.FileCMO, buildDir </> cmo_of_module false false m)) allMods in

        let targetNodes =
            match target.target_type with
            | Target.Lib ->
                let libNatNode = file_id (Filetype.FileCMXA, buildDir </> cmxa_of_lib target.target_name) in
                let libByteNode = file_id (Filetype.FileCMA, buildDir </> cma_of_lib target.target_name) in
                Dag.addChildrenEdges libNatNode allCmx filesDag;
                Dag.addChildrenEdges libByteNode allCmo filesDag;
                [libNatNode; libByteNode]
                
            | _           ->
                let exePath = buildDir </> Utils.to_exe_name target.target_name in
                let exeNode = file_id (Filetype.FileEXE, exePath) in
                Dag.addChildrenEdges exeNode allCmx filesDag;
                [exeNode]
            in

        (* TODO add all C objects and archives to the DAG as dependency of target
         * before we dagify the target's linking *)
        ();

        (stepsDag, filesDag)
        in
    (* prepare the module hashtbl *)
    List.iter (fun m -> loop m) toplevelModules;

    let (dag, fdag) = get_dags () in

    if bstate.bstate_config.project_buildopts.opt_dump_dot
        then (
            let dotDir = Dist.createBuildDest Dist.Dot in
            let path = dotDir </> fn (Target.get_target_name target ^ ".dot") in
            let dotContent = Dag.toDot (fun x -> match x with
                                                 | CompileModule x    -> "mod " ^ x.modname
                                                 | CompileInterface x -> "intf " ^ x.modname
                                                 | CompileC x         -> "C " ^ x.filename
                                       ) target.target_name true dag in
            Filesystem.writeFile path dotContent;

            let path = dotDir </> fn (Target.get_target_name target ^ ".files.dot") in
            let dotContent = Dag.toDot (fun fdep -> Filetype.file_type_to_string fdep.fdep_ty ^ " " ^ fp_to_string fdep.fdep_path)
                                       target.target_name true fdag in
            Filesystem.writeFile path dotContent;

        );

    { compilation_modules  = modulesDeps
    ; compilation_csources = target.target_csources
    ; compilation_dag      = dag
    ; compilation_filesdag = fdag
    ; compilation_builddir = buildDir
    ; compilation_paths    = [buildDir] @ (maybe [] (fun v -> [v]) target.target_srcdir)
    }
