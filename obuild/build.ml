open Ext.Fugue
open Ext.Filepath
open Ext
open Types
open Helper
open Process
open Printf
open Filetype
open Analyze
open Target
open Prepare
open Gconf
open Modname
open Hier
open Buildprogs
open Dependencies
open Pp

exception CCompilationFailed of string
exception CompilationFailed of string
exception Internal_Inconsistancy of string * string

(* check that destination is valid (mtime wise) against a list of srcs and
 * if not valid gives the filepath that has changed.
 * *)
let check_destination_valid_with srcs cstate (filety, dest) =
    if Filesystem.exists dest
        then (
            let destTime = Filesystem.getModificationTime dest in
            try Some (List.find (fun (_,path) ->
                let mtime = Filesystem.getModificationTime path in
                destTime < mtime
                ) srcs)
            with Not_found -> None
        ) else
            Some (FileO, currentDir)

(* same as before but the list of sources is automatically determined
 * from the file DAG
 *)
let check_destination_valid cstate (filety, dest) =
    let children =
        try Dag.getChildren cstate.compilation_filesdag (file_id (filety, dest))
        with Dag.DagNode_Not_found ->
            raise (Internal_Inconsistancy ((file_type_to_string filety), ("missing destination: " ^ fp_to_string dest)))
        in
    check_destination_valid_with (List.map un_file_id children) cstate (filety,dest)

(* get a nice reason of why a destination is not deemed valid against
 * the source filepath that triggered the unvalid check.
 *
 * if source filepath is empty, it means that destination doesn't exists *)
let reason_from_paths (_,dest) (srcTy,changedSrc) =
    let trim_pd_exts z =
        let n = fn_to_string z in
        if string_endswith ".d" n then fn (Filename.chop_suffix n ".d")
        else if string_endswith ".p" n then fn (Filename.chop_suffix n ".p")
        else z
        in
    if changedSrc = currentDir
        then ""
        else (
            let bdest = path_basename dest in
            let bsrc  = path_basename changedSrc  in
            match Filetype.get_extension bdest with
            | FileCMX | FileCMO -> (
                match srcTy with
                | FileCMX | FileCMO ->
                    let bml = Filetype.replace_extension bdest FileML in
                    let bmli = Filetype.replace_extension bdest FileMLI in
                    if bml = bsrc then "Source changed"
                    else if bmli = bsrc then "Interface changed"
                    else ("Dependency " ^ modname_to_string (module_of_filename (trim_pd_exts bsrc)) ^ " changed " ^ fp_to_string changedSrc)
                | FileCMXA | FileCMA ->
                    "Library changed " ^ fp_to_string changedSrc
                | _ ->
                    "Dependencies changed " ^ fp_to_string changedSrc
                )
            | FileO ->
                let bc = Filetype.replace_extension bdest FileC in
                let bh = Filetype.replace_extension bdest FileH in
                if bc = bsrc then ("C file " ^ fn_to_string bsrc ^ " changed")
                else if bh = bsrc then ("H file " ^ fn_to_string bsrc ^ " changed")
                else ("file changed " ^ fp_to_string changedSrc)
            | _ ->
                fp_to_string changedSrc ^ " changed"
        )

(* compile will process the compilation DAG,
 * which will compile all C sources and OCaml modules.
 *)
let compile_ (bstate: build_state) (cstate: compilation_state) target =
    let annotMode =
        if gconf.conf_annot && gconf.conf_bin_annot then AnnotationBoth
        else if gconf.conf_annot then AnnotationText
        else if gconf.conf_bin_annot then AnnotationBin
        else AnnotationNone
        in
    let compileOpts = Target.get_compilation_opts target in
    let cbits = target.target_cbits in

    let nbStep = Dag.length cstate.compilation_dag in
    let nbStepLen = String.length (string_of_int nbStep) in
    let taskdep = Taskdep.init cstate.compilation_dag in

    let on_task_finish task =
        Taskdep.markDone taskdep task;
        match task with
        | CompileModule m -> ()
        | _               -> ()
        in

    let cDirSpec =
        { include_dirs = cstate.compilation_c_include_paths
        ; dst_dir      = cstate.compilation_builddir_c
        ; src_dir      = cbits.target_cdir
        }
        in
    (* add a C compilation process *)
    let compile_c taskIndex task cFile =
        let dest = (FileO, cDirSpec.dst_dir </> o_from_cfile cFile) in
        (match check_destination_valid cstate dest with
        | None            -> on_task_finish task; Retry
        | Some srcChanged ->
            let reason = reason_from_paths dest srcChanged in
            verbose Report "[%*d of %d] Compiling C %-.30s%s\n%!" nbStepLen taskIndex nbStep (fn_to_string cFile)
                    (if reason <> "" then "    ( " ^ reason ^ " )" else "");
            let cflags = cbits.target_cflags in
            AddProcess (task, runCCompile bstate.bstate_config cDirSpec cflags cFile)
        )
        in

    let buildModes = Target.get_ocaml_compiled_types target in

    let buildmode_to_filety bmode = if bmode = Native then FileCMX else FileCMO in
    let buildmode_to_library_filety bmode = if bmode = Native then FileCMXA else FileCMA in

    let selfDeps = Analyze.get_internal_library_deps bstate.bstate_config target in
    let internalLibsPathsAllModes =
        List.map (fun (compileOpt,compileType) ->
            ((compileOpt,compileType), List.map (fun dep ->
                let dirname = Dist.getBuildDest (Dist.Target (LibName dep)) in
                let filety = buildmode_to_library_filety compileType in
                let libpath = dirname </> cmca_of_lib compileType compileOpt dep in
                (filety, libpath)
            ) selfDeps)
        ) [ (Normal,Native);(Normal,ByteCode);(WithProf,Native);(WithProf,ByteCode);(WithDebug,Native);(WithDebug,ByteCode)]

        in

    (* add a OCaml module or interface compilation process *)
    let compile_module taskIndex task isIntf h =
        let packOpt = hier_parent h in
        let hdesc =
            let desc = Hashtbl.find cstate.compilation_modules h in
            match desc.module_ty with
            | DescFile z -> z
            | DescDir _  -> failwith (sprintf "internal error compile module on directory (%s). steps dag internal error" (hier_to_string h))
            in
        let srcPath = path_dirname hdesc.module_src_path in

        let useThread = hdesc.module_use_threads in
        let dirSpec =
            { src_dir      = srcPath
            ; dst_dir      = currentDir
            ; include_dirs = [currentDir]
            }
            in
        let moduleDeps = hdesc.dep_cwd_modules in

        let depDescs =
            if isIntf
            then (
                let intfDesc =
                    match hdesc.module_intf_desc with
                    | None      -> failwith "assertion error, task interface and no module_intf"
                    | Some intf -> intf
                    in
                List.map (fun compOpt ->
                    let dest = (FileCMI, cstate.compilation_builddir_ml compOpt <//> cmi_of_hier h) in
                    let src  = [ (FileMLI, intfDesc.module_intf_path) ] in
                    let mDeps = List.map (fun moduleDep ->
                        (FileCMI, cstate.compilation_builddir_ml compOpt <//> cmi_of_hier moduleDep)
                    ) moduleDeps in
                    let internalDeps = List.assoc (compOpt,ByteCode) internalLibsPathsAllModes in
                    (dest,Interface,compOpt, src @ internalDeps @ mDeps)
                ) compileOpts
            ) else (
                (* TODO: need way to not do debug/profile per (native|bytecode) mode *)
                let allModes = List.concat
                    (List.map (fun compiledTy -> List.map (fun cmode -> (compiledTy, cmode)) compileOpts) buildModes)
                    in
                List.map (fun (compiledTy, compOpt) ->
                    let fileCompileTy = buildmode_to_filety compiledTy in
                    let dest = (fileCompileTy, cstate.compilation_builddir_ml compOpt <//> cmc_of_hier compiledTy h) in
                    let src  =
                          (match hdesc.module_intf_desc with None -> [] | Some intf -> [FileMLI,intf.module_intf_path])
                        @ [(FileML, hdesc.module_src_path)] in
                    let mDeps = List.concat (List.map (fun moduleDep ->
                        [(fileCompileTy, cstate.compilation_builddir_ml compOpt <//> cmc_of_hier compiledTy moduleDep)
                        ;(FileCMI, cstate.compilation_builddir_ml compOpt <//> cmi_of_hier moduleDep)
                        ]
                    ) moduleDeps) in
                    let internalDeps = List.assoc (compOpt,compiledTy) internalLibsPathsAllModes in
                    (dest,Compiled compiledTy,compOpt,src @ internalDeps @ mDeps)
                ) allModes
            )
            in

        let rec check invalid descs =
            match descs with
            | []                                  -> (None, [])
            | (dest,buildMode,compOpt,srcs) :: xs ->
                let rDirSpec = { dirSpec with dst_dir = cstate.compilation_builddir_ml compOpt <//> hier_to_dirpath h
                                            ; include_dirs = cstate.compilation_include_paths compOpt h } in
                let fcompile =
                    (buildMode,
                    (fun () -> runOcamlCompile rDirSpec useThread annotMode buildMode compOpt packOpt hdesc.module_use_pp (hier_leaf h))) in
                if invalid
                    then (
                        let (_, ys) = check invalid xs in
                        (Some "", fcompile :: ys)
                    ) else (
                        match check_destination_valid_with srcs cstate dest with
                        | None            -> check false xs
                        | Some srcChanged ->
                            let reason = reason_from_paths dest srcChanged in
                            let (_, ys) = check true xs in
                            (Some reason, fcompile :: ys)
                    )
            in
        let (compilationReason, checkFunList) = check false depDescs in
        match compilationReason with
        | None        -> on_task_finish task; Retry
        | Some reason ->
                (* if we module has an interface, we create one list, so everything can be run in parallel,
                 * otherwise we partition the buildMode functions in buildModes group. *)
                let funLists =
                    if isIntf || module_file_has_interface hdesc
                        then [List.map snd checkFunList]
                        else let (l1,l2) = List.partition (fun (x,_) -> x = Compiled Native) checkFunList in
                             List.filter (fun x -> List.length x > 0) [List.map snd l1; List.map snd l2]
                    in
                let verb = if isIntf then "Intfing" else "Compiling" in
                verbose Report "[%*d of %d] %s %-.30s%s\n%!" nbStepLen taskIndex nbStep verb (hier_to_string h)
                    (if reason <> "" then "    ( " ^ reason ^ " )" else "");
                AddTask (task, funLists)
        in
    (* compile a set of modules in directory into a pack *)
    let compile_directory taskIndex task h =
        let packOpt = hier_parent h in

        (* get all the modules defined at level h+1 *)
        let modulesTask = Taskdep.linearize cstate.compilation_dag Taskdep.FromParent [task] in
        let filterModules t : hier option =
            match t with
            | (CompileC _)         -> None
            | (CompileInterface _) -> None
            | (CompileDirectory m) -> if hier_lvl m = (hier_lvl h + 1) then Some m else None
            | (CompileModule m)    -> if hier_lvl m = (hier_lvl h + 1) then Some m else None
            in
        let modules = List.rev $ list_filter_map filterModules modulesTask in

        (* directory never have interface (?) so we serialize the native/bytecode creation.
         * the mtime checking is sub-optimal. low hanging fruits warning *)
        let tasksOps : (string * spawn) option list list =
            (List.map (fun buildMode ->
                List.map (fun compOpt ->
                    let dest = (FileCMI, cstate.compilation_builddir_ml compOpt <//> cmi_of_hier h) in
                    let mdeps = List.map (fun m ->
                            (FileCMI, cstate.compilation_builddir_ml compOpt <//> cmi_of_hier m)
                        ) modules in
                    let dir = cstate.compilation_builddir_ml compOpt in
                    let fcompile = (fun () -> runOcamlPack dir dir buildMode packOpt h modules) in
                    match check_destination_valid_with mdeps cstate dest with
                    | None            -> None
                    | Some srcChanged -> Some (reason_from_paths dest srcChanged, fcompile)
                ) compileOpts
            ) buildModes)
            in
        let (reason, ops) =
            (*[ [(r,f)] ]*)
            let l : (string * spawn) list list = List.map maybes_to_list tasksOps in
            match List.filter (fun x -> x <> []) l with
            | []                -> ("", [])
            | [] :: ys          -> assert false
            | ((r,x)::xs) :: ys -> (r, (x :: List.map snd xs) :: List.map (List.map snd) ys)
            in
        if ops <> []
            then (
                verbose Report "[%*d of %d] Packing %-.30s%s\n%!" nbStepLen taskIndex nbStep (hier_to_string h) reason;
                AddTask (task, ops)
            ) else (
                on_task_finish task;
                Retry
            )
        in
    (* a compilation task has finished, terminate the process,
     * and process the result
     *)
    let schedule_finish (task, st) isDone =
        (match terminate_process (task, st) with
        | Success (_, warnings) -> (* TODO: store warnings for !isDone and print them if they are different when isDone *)
                                    if isDone then print_warnings warnings
        | Failure er            -> match task with
                                   | CompileC _ -> raise (CCompilationFailed er)
                                   | _          -> raise (CompilationFailed er)
        );
        if isDone then
            on_task_finish task
        in

    (* when the scheduler has some room, we get the next task from
     * taskdep and either start a process or call retry.
     *
     * Retry is returned when no process need to be spawned for the next task
     * since the dependencies have not changed and thus the cache still have
     * valid target file. Instead of returning retry, we could just go get
     * the next task ourself.
     *)
    let schedule_idle () =
        let dispatch (taskIndex, task) =
            match task with
            | (CompileC m)         -> compile_c taskIndex task m
            | (CompileInterface m) -> compile_module taskIndex task true m
            | (CompileModule m)    -> compile_module taskIndex task false m
            | (CompileDirectory m) -> compile_directory taskIndex task m
            in
        if Taskdep.isComplete taskdep
            then Terminate
            else match Taskdep.getnext taskdep with
                 | None      -> WaitingTask
                 | Some task -> dispatch task
        in

    let stat = schedule gconf.conf_parallel_jobs schedule_idle schedule_finish in
    verbose Verbose "schedule finished: #processes=%d max_concurrency=%d\n" stat.nb_processes stat.max_runqueue;
    ()

let compile bstate cstate target =
    compile_ bstate cstate target

let linkCStuff bstate cstate clibName =
    let soFile = cstate.compilation_builddir_c </> fn ("dll" ^ clibName ^ ".so") in
    let aFile = cstate.compilation_builddir_c </> fn ("lib" ^ clibName ^ ".a") in

    let cdepFiles = List.map (fun x -> cstate.compilation_builddir_c </> o_from_cfile x) cstate.compilation_csources in
    print_warnings (runCLinking LinkingShared cdepFiles soFile);
    print_warnings (runAr aFile cdepFiles);
    print_warnings (runRanlib aFile);
    ()

let linking bstate cstate target =
    let compile_opts = Target.get_compilation_opts target in
    let compiledTypes = Target.get_ocaml_compiled_types target in

    let cbits = target.target_cbits in

    let compiled = get_compilation_order cstate in
    verbose Debug "  compilation order: %s\n" (Utils.showList "," hier_to_string compiled);

    let selfDeps = Analyze.get_internal_library_deps bstate.bstate_config target in
    verbose Debug "  self deps: %s\n" (Utils.showList "," lib_name_to_string selfDeps);
    let selfLibDirs = List.map (fun dep -> Dist.getBuildDest (Dist.Target (LibName dep))) selfDeps in

    let internal_cclibs =
        if cstate.compilation_csources <> []
            then [Target.get_target_clibname target]
            else []
        in
    let cclibs = List.concat (List.map (fun (cpkg,_) -> List.map (fun x -> "-l" ^ x) (Analyze.get_c_pkg cpkg bstate.bstate_config).cpkg_conf_libs) cbits.target_cpkgs)
               @ List.map (fun x -> "-L" ^ fp_to_string x) selfLibDirs
               @ List.map (fun x -> "-l" ^ x) (cbits.target_clibs @ internal_cclibs)
        in

    let pkgDeps = Analyze.get_pkg_deps target bstate.bstate_config in
    verbose Verbose "package deps: [%s]\n" (Utils.showList "," lib_name_to_string pkgDeps);

    let useThreadLib =
        if List.mem (lib_name_of_string "threads") pkgDeps
        || List.mem (lib_name_of_string "threads.posix") pkgDeps
            then WithThread
            else NoThread
        in
    let useThreads = useThreadLib in

    if cstate.compilation_csources <> [] then (
        let cname = Target.get_target_clibname target in
        linkCStuff bstate cstate cname;
    );

    List.iter (fun compiledType ->
        List.iter (fun compileOpt ->
            let buildDeps =
                if is_target_lib target
                    then []
                    else
                        list_filter_map (fun dep ->
                            match Hashtbl.find bstate.bstate_config.project_dep_data dep with
                            | Internal -> Some (in_current_dir (cmca_of_lib compiledType compileOpt dep))
                            | System   ->
                                let meta = Analyze.get_pkg_meta dep bstate.bstate_config in
                                let pred =
                                    match compiledType with
                                    | Native    -> Meta.Pred_Native
                                    | ByteCode  -> Meta.Pred_Byte
                                    in
                                let archives = Meta.getArchiveWithFilter meta dep pred in
                                match archives with
                                | []              -> None
                                | archiveFile::_  -> Some (in_current_dir $ fn (snd archiveFile))
                        ) pkgDeps
                in

            let dest =
                match target.target_name with
                | LibName libname ->
                    cstate.compilation_builddir_ml Normal </> cmca_of_lib compiledType compileOpt libname
                | _ ->
                    let outputName = Utils.to_exe_name compileOpt compiledType (Target.get_target_dest_name target) in
                    cstate.compilation_builddir_ml Normal </> outputName
                in
            let linking_paths_of compileOpt =
                match compileOpt with
                | Normal    -> cstate.compilation_linking_paths
                | WithDebug -> cstate.compilation_linking_paths_d
                | WithProf  -> cstate.compilation_linking_paths_p
                in

            let destTime = Filesystem.getModificationTime dest in
            let depsTime =
                try Some (List.find (fun p -> destTime < Filesystem.getModificationTime p) (List.map (fun m -> cstate.compilation_builddir_ml compileOpt <//> cmc_of_hier compiledType m) compiled))
                with Not_found -> None
                in
            if depsTime <> None then (
                verbose Report "Linking %s %s\n%!" (if is_target_lib target then "library" else "executable") (fp_to_string dest);

                print_warnings (runOcamlLinking
                    (linking_paths_of compileOpt)
                    compiledType
                    (if is_target_lib target then LinkingLibrary else LinkingExecutable)
                    compileOpt
                    useThreads
                    cclibs
                    buildDeps
                    compiled
                    dest)
            )
        ) compile_opts
    ) compiledTypes

let get_destination_files target =
    let compileOpts = Target.get_compilation_opts target in
    let compiledTypes = Target.get_ocaml_compiled_types target in
    match target.Target.target_name with
    | LibName libname ->
        List.concat (List.map (fun compiledType ->
            List.map (fun compileOpt -> cmca_of_lib compiledType compileOpt libname) compileOpts
        ) compiledTypes)
    | ExeName e | TestName e | BenchName e | ExampleName e ->
        List.concat (List.map (fun compiledType ->
            List.map (fun compileOpt ->
                Utils.to_exe_name compileOpt compiledType (Target.get_target_dest_name target)
            ) compileOpts
        ) compiledTypes)

let sanity_check buildDir target =
    let files = get_destination_files target in
    let allOK = List.for_all (fun f -> Filesystem.exists (buildDir </> f)) files in
    if not allOK
        then verbose Report "warning: some target file appears to be missing";
    ()

let buildTarget bstate target modules =
    let buildDir = Dist.createBuildDest (Dist.Target target.target_name) in
    let cstate = prepare_target bstate buildDir target modules in
    compile bstate cstate target;
    linking bstate cstate target;
    sanity_check buildDir target;
    ()

let buildLib bstate lib =
    verbose Report "Building library %s\n" (lib_name_to_string lib.Project.lib_name);
    buildTarget bstate (Project.lib_to_target lib) lib.Project.lib_modules

let buildExe bstate exe =
    verbose Report "Building executable %s\n" (exe.Project.exe_name);
    buildTarget bstate (Project.exe_to_target exe) [module_of_filename exe.Project.exe_main]

let buildTest bstate test =
    verbose Report "Building test %s\n" (test.Project.test_name);
    buildTarget bstate (Project.test_to_target test) [module_of_filename test.Project.test_main]

let buildBench bstate bench =
    verbose Report "Building benchmark %s\n" (bench.Project.bench_name);
    buildTarget bstate (Project.bench_to_target bench) [module_of_filename bench.Project.bench_main]

let buildExample bstate example =
    verbose Report "Building example %s\n" (example.Project.example_name);
    buildTarget bstate (Project.example_to_target example) [module_of_filename example.Project.example_main]
