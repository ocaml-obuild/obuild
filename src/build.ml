open Types
open Ext
open Helper
open Process
open Printf
open Filepath
open Filetype
open Prepare
open Target
open Building
open Gconf
open Modname

exception CCompilationFailed of string
exception CompilationFailed of string
exception LinkingFailed of string
exception Internal_Inconsistancy of string * string

type compile_opt =
    { compile_common        : common_opts
    ; compile_dest_dir      : filepath
    ; compile_src_dir       : filepath option
    ; compile_use_thread    : bool
    ; compile_include_paths : filepath list
    }

type ccompile_opt =
    { ccompile_common        : common_opts
    ; ccompile_opts          : string list
    ; ccompile_include_paths : filepath list
    ; ccompile_dest_dir      : filepath
    ; ccompile_src_dir       : filepath option
    }

type clinking_opt =
    { clinking_common        : common_opts
    ; clinking_shared        : bool
    ; clinking_dest          : filepath
    }

type linking_opts =
    { linking_common        : common_opts
    ; linking_lib           : bool
    ; linking_dest          : filepath
    ; linking_use_thread    : bool
    ; linking_cclibs        : string list
    ; linking_include_paths : filepath list
    }

let runOcamlCompile copt compileInterface useDebug useProf modname =
    let commonOpts = copt.compile_common in
    let (srcFile, dstFile) =
        if compileInterface
            then
                (with_optpath copt.compile_src_dir (interface_of_module modname)
                ,copt.compile_dest_dir </> (cmi_of_module modname)
                )
            else 
                (with_optpath copt.compile_src_dir (filename_of_module modname)
                ,copt.compile_dest_dir </> ((cmc_of_module commonOpts.common_build_native useDebug useProf) modname)
                )
        in
    let args = [ if commonOpts.common_build_native then Prog.getOcamlOpt () else Prog.getOcamlC () ]
             @ (if copt.compile_use_thread then ["-thread"] else [])
             @ (Utils.to_include_path_options copt.compile_include_paths)
             @ (if useProf then [ "-p"] else [])
             @ (if useDebug then [ "-g"] else [])
             @ ["-o"; fp_to_string dstFile ]
             @ ["-c"; fp_to_string srcFile ]
        in
    spawn args

let o_from_cfile file = fn (file.filename ^ ".o")

let runCCompile project copt file =
    let setup = project.project_ocamlcfg in

    let callCCompiler =
        try string_words_noempty (Hashtbl.find setup "bytecomp_c_compiler")
        with Not_found -> raise (CCompilationFailed "no bytecomp_c_compiler returned by ocaml")
        in
    let srcFile = with_optpath copt.ccompile_src_dir file in
    (* make a .c.o file to avoid collision *)
    let dstFile = copt.ccompile_dest_dir </> o_from_cfile file in
    let args = callCCompiler
             @ copt.ccompile_opts
             @ (Utils.to_include_path_options copt.ccompile_include_paths)
             @ ["-o"; dstFile.filepath]
             @ ["-c"; srcFile.filepath]
        in
    spawn args

let runCLinking lopt depfiles =
    let args = [ Prog.getCC () ]
             @ (if lopt.clinking_shared then [ "-shared" ] else [])
             @ ["-o"; fp_to_string lopt.clinking_dest ]
             @ List.map fp_to_string depfiles
             in
    match run_with_outputs args with
    | Success (_, warnings) -> warnings
    | Failure er            -> raise (LinkingFailed er)

let runAr dest deps =
    let args = [ Prog.getAR (); "rc"; fp_to_string dest ] @ List.map fp_to_string deps in
    match run_with_outputs args with
    | Success (_, warnings) -> warnings
    | Failure er            -> raise (LinkingFailed er)

let runRanlib dest =
    match run_with_outputs [ Prog.getRanlib (); fp_to_string dest ] with
    | Success (_, warnings) -> warnings
    | Failure er            -> raise (LinkingFailed er)


let runOcamlLinking lopt libs modules =
    let commonOpts = lopt.linking_common in
    let args = [ if commonOpts.common_build_native then Prog.getOcamlOpt () else Prog.getOcamlC () ]
             @ (if lopt.linking_use_thread then ["-thread"] else [])
             @ (if lopt.linking_lib then ["-a"] else [])
             @ [ "-o"; lopt.linking_dest.filepath ]
             @ (Utils.to_include_path_options lopt.linking_include_paths)
             @ (List.map fp_to_string libs)
             @ (List.map fn_to_string $ List.map (cmc_of_module commonOpts.common_build_native false false) modules)
             @ (List.concat (List.map (fun x -> [ "-cclib"; x ]) lopt.linking_cclibs))
             in
    match run_with_outputs args with
    | Success (_, warnings) -> warnings
    | Failure er            -> raise (LinkingFailed er)

let check_destination_valid cstate filety (dest:filepath) =
    let children =
        try Dag.getChildren cstate.compilation_filesdag (file_id (filety, dest))
        with Dag.DagNode_Not_found ->
            raise (Internal_Inconsistancy ((file_type_to_string filety), ("missing destination: " ^ fp_to_string dest)))
        in
    let fps = List.map (fun x -> x.fdep_path) children in
    if Filesystem.exists dest
        then (
            let destTime = Filesystem.getModificationTime dest in
            try Some (List.find (fun path ->
                let mtime = Filesystem.getModificationTime path in
                destTime < mtime
                ) fps)
            with Not_found -> None
        ) else
            Some (fp "")

(* get a nice reason out of 2 filepath *)
let reason_from_paths dest changedSrc =
    if changedSrc.filepath = ""
        then ""
        else (
            let bdest = path_basename dest in
            let bsrc  = path_basename changedSrc  in
            match Filetype.get_extension bdest with
            | FileCMX | FileCMO ->
                let bml = Filetype.replace_extension bdest FileML in
                let bmli = Filetype.replace_extension bdest FileMLI in
                if bml = bsrc then "source changed"
                else if bmli = bsrc then "interface changed"
                else ("module " ^ (module_of_filename bsrc).modname ^ " changed")
            | FileO ->
                let bc = Filetype.replace_extension bdest FileC in
                let bh = Filetype.replace_extension bdest FileH in
                if bc = bsrc then ("C file " ^ fn_to_string bsrc ^ " changed")
                else if bh = bsrc then ("H file " ^ fn_to_string bsrc ^ " changed")
                else ("file changed " ^ fp_to_string changedSrc)
            | _ ->
                fp_to_string changedSrc ^ " changed"
        )

let separate_deps project =
    let internal = ref [] in
    let system = ref [] in
    Hashtbl.iter (fun k dep_type ->
        if dep_type = Internal
            then internal := k :: !internal
            else system   := k :: !system
    ) project.project_dep_data;
    (!internal, !system)

let get_caml_include_path (project: project_config) =
    try fp (Hashtbl.find project.project_ocamlcfg "standard_library")
    with Not_found -> raise (CCompilationFailed "no standard_library returned by ocaml")

let get_dep_path (bstate: build_state) dep =
    let camlIncludePath = get_caml_include_path bstate.bstate_config in
    match (Hashtbl.find bstate.bstate_config.project_dep_data dep.dep_name) with
    | Internal -> Dist.createBuildDest (Dist.Library dep.dep_name)
    | System _ -> with_path camlIncludePath (fn dep.dep_name)

(* compile will process the compilation DAG,
 * which will compile all C sources and OCaml modules.
 *)
let compile (bstate: build_state) (cstate: compilation_state) target =
    let compileDebug = true in
    let compileProfile = true in

    let camlIncludePath = get_caml_include_path bstate.bstate_config in

    let extraPaths = List.map (fun (dep,_) -> get_dep_path bstate dep) target.target_builddeps in

    let compiledModules = ref [] in

    let nbStep = Dag.length cstate.compilation_dag in
    let taskdep = Dag.taskdep_init cstate.compilation_dag in

    let on_task_finish task =
        Dag.taskdep_markDone taskdep task;
        match task with
        | CompileModule m    -> compiledModules := m :: !compiledModules
        | CompileInterface _ -> ()
        | CompileC _         -> ()
        in

    let cCopt =
        { ccompile_common        = bstate.bstate_config.project_commonopts
        ; ccompile_opts          = target.target_copts
        ; ccompile_include_paths = (maybe [] (fun x -> [x]) target.target_cdir) @ [camlIncludePath]
        ; ccompile_dest_dir      = cstate.compilation_builddir
        ; ccompile_src_dir       = target.target_cdir
        }
        in
    let moduleCopt =
        { compile_common        = bstate.bstate_config.project_commonopts
        ; compile_dest_dir      = cstate.compilation_builddir
        ; compile_src_dir       = target.target_srcdir
        ; compile_use_thread    = false
        ; compile_include_paths = cstate.compilation_paths @ extraPaths
        }
        in

    (* add a C compilation process *)
    let compile_c taskIndex task cFile =
        let dest = cCopt.ccompile_dest_dir </> o_from_cfile cFile in
        (match check_destination_valid cstate FileO dest with
        | None            -> on_task_finish task; Retry
        | Some srcChanged ->
            let reason = reason_from_paths dest srcChanged in
            verbose Report "[%d of %d] Compiling C %s%s\n%!" taskIndex nbStep (fn_to_string cFile)
                    (if reason <> "" then "    ( " ^ reason ^ " )" else "");
            AddProcess (task, runCCompile bstate.bstate_config cCopt cFile)
        )
        in

    (* add a OCaml module or interface compilation process *)
    let compile_module taskIndex task isIntf m =
        let useThreads = (Hashtbl.find cstate.compilation_modules m).module_use_threads in
        let copt = if useThreads then { moduleCopt with compile_use_thread = true } else moduleCopt in

        (*match check_dest_is_valid *)
        let dest = copt.compile_dest_dir </> cmx_of_module false false m in
        (match check_destination_valid cstate FileCMX dest with
        | None            -> on_task_finish task; Retry
        | Some srcChanged -> (
                let reason = reason_from_paths dest srcChanged in
                let verb = if isIntf then "Intfing" else "Compiling" in
                verbose Report "[%d of %d] %s %s%s\n%!" taskIndex nbStep verb (m.modname)
                    (if reason <> "" then "    ( " ^ reason ^ " )" else "");
                if isIntf
                    then AddProcess (task, runOcamlCompile copt isIntf false false m)
                    else AddTask (task,
                              [fun () -> runOcamlCompile copt isIntf false false m]
                            @ (if compileDebug then [fun () -> runOcamlCompile copt isIntf true false m] else [])
                            @ (if compileProfile then [fun () -> runOcamlCompile copt isIntf false true m] else [])
                            )
                )
        )
        in
    (* a compilation task has finished, terminate the process,
     * and process the result
     *)
    let schedule_finish (task, st) isDone =
        (match terminate_process (task, st) with
        | Success (_, warnings) -> if isDone then print_warnings warnings
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
            in
        if Dag.taskdep_isComplete taskdep
            then Terminate
            else match Dag.taskdep_getnext taskdep with
                 | None      -> WaitingTask
                 | Some task -> dispatch task
        in

    schedule bstate.bstate_config.project_buildopts.opt_nb_jobs_par schedule_idle schedule_finish;
    List.rev !compiledModules

let linkCStuff bstate cstate clibName =
    let soFile = cstate.compilation_builddir </> fn ("dll" ^ clibName ^ ".so") in
    let aFile = cstate.compilation_builddir </> fn ("lib" ^ clibName ^ ".a") in

    let clopts = { clinking_common = bstate.bstate_config.project_commonopts
                 ; clinking_shared = true
                 ; clinking_dest   = soFile
                 } in
    let cdepFiles = List.map (fun x -> cstate.compilation_builddir </> fn (fn_to_string x ^ ".o")) cstate.compilation_csources in
    print_warnings (runCLinking clopts cdepFiles);
    print_warnings (runAr aFile cdepFiles);
    print_warnings (runRanlib aFile);
    ()

let linking bstate cstate target compiled =
    let extraPaths =
        if is_target_lib target
            then []
            else List.map (fun (dep,_) -> get_dep_path bstate dep) target.target_builddeps
        in

    let (selfDeps, systemDeps) = separate_deps bstate.bstate_config in
    let selfLibDirs = List.map (fun dep -> Dist.createBuildDest (Dist.Library dep)) selfDeps in

    let dest =
        if is_target_lib target
            then (
                with_path cstate.compilation_builddir (cmxa_of_lib target.target_name)
            ) else (
                let outputName = Utils.to_exe_name target.target_name in
                cstate.compilation_builddir </> outputName
            )
        in
    verbose Report "Linking %s\n%!" (fp_to_string dest);

    let useThreadLib = List.mem "threads" $ List.map (fun (dep,_) -> dep.dep_name) target.target_builddeps in
    let useThreads = Hashtbl.fold (fun _ v a -> v.module_use_threads || a) cstate.compilation_modules useThreadLib in

    let cclibs =
        if is_target_lib target
            then (if cstate.compilation_csources <> [] then [target.target_name] else [])
            else []
        in

    let lopts = { linking_common        = bstate.bstate_config.project_commonopts
                ; linking_lib           = is_target_lib target
                ; linking_dest          = dest
                ; linking_use_thread    = useThreads
                ; linking_cclibs        = List.map (fun x -> "-L" ^ fp_to_string x) selfLibDirs
                                        @ List.map (fun x -> "-l" ^ x) (target.target_clibs @ cclibs)
                ; linking_include_paths = [cstate.compilation_builddir] @ extraPaths
                } in

    let childrenDeps_wrapped = Dag.getChildren_full bstate.bstate_config.project_deps_dag (Prepare.Target target.target_name) in
    let childrenDeps =
        List.fold_left (fun acc t ->
            match t with
            | Prepare.Target _       -> acc
            | Prepare.Dependency dep -> dep :: acc
        ) [] childrenDeps_wrapped
        in

    let buildDeps =
        if is_target_lib target
            then []
            else List.map (fun dep ->
                    (* map to include path and a cma/cmxa *)
                    let path = get_dep_path bstate dep in
                    (* FIXME. file name is "hardcoded"
                     * proper way is not to call cmxa_of_lib, but look into the META file
                     * for the archive line for native or bytecode
                     *)
                    in_current_dir (cmxa_of_lib dep.dep_name)
                ) childrenDeps
        in
    if is_target_lib target then (
        if cstate.compilation_csources <> []
            then linkCStuff bstate cstate target.target_name; (* FIXME should be only the library subname *)
    );

    print_warnings (runOcamlLinking lopts buildDeps compiled)

(* compile and link a library
 *)
let buildLib bstate lib =
    verbose Report "Building library %s\n" lib.Project.lib_name;
    let target = Project.lib_to_target lib in
    let buildDir = Dist.createBuildDest (Dist.Library lib.Project.lib_name) in
    let cstate = prepare_target bstate buildDir target lib.Project.lib_modules in
    let compiled = compile bstate cstate target in
    verbose Report "Linking library %s\n" lib.Project.lib_name;
    linking bstate cstate target compiled;
    ()

(* compile and link an executable
 *
 * most of the preparation is done in prepare,
 * here we compile every modules and link the result at the end.
 *
 *)
let buildExe bstate exe =
    verbose Report "Building executable %s\n" (exe.Project.exe_name);
    let target = Project.exe_to_target exe in
    let buildDir = Dist.createBuildDest (Dist.Executable exe.Project.exe_name) in
    (* get modules dependencies *)
    let cstate   = prepare_target bstate buildDir target [module_of_filename (fn exe.Project.exe_main)] in
    let compiled = compile bstate cstate target in
    linking bstate cstate target compiled;
    ()
