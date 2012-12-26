open Types
open Ext
open Helper
open Printf

exception CompilationFailed of string
exception DependencyAnalyzeFailed of string
exception LinkingFailed of string

type module_desc =
    { module_use_threads : bool
    ; module_src_mtime : float
    ; module_has_interface : bool
    ; dep_cwd_modules : modname list
    ; dep_other_modules : modname list
    }

type dep_opt =
    { dep_withopt : bool
    ; dep_src_dir : string option
    }

type compile_opt =
    { compile_native        : bool (* build native or bytecode *)
    ; compile_withopt       : bool (* use the .opt compiler *)
    ; compile_dest_dir      : string option
    ; compile_src_dir       : string option
    ; compile_use_thread    : bool
    ; compile_include_paths : string list
    }

type linking_opts =
    { linking_native        : bool
    ; linking_withopt       : bool
    ; linking_dest          : string
    ; linking_use_thread    : bool
    ; linking_include_paths : string list
    }

let cmxa_of_lib lib = lib ^ ".cmxa"
let cma_of_lib lib = lib ^ ".cma"

let cmx_of_module modname = String.uncapitalize modname ^ ".cmx"
let cmo_of_module modname = String.uncapitalize modname ^ ".cmo"
let cmi_of_module modname = String.uncapitalize modname ^ ".cmi"
let o_of_module modname = String.uncapitalize modname ^ ".o"

let filename_of_module modname = String.uncapitalize modname ^ ".ml"
let interface_of_module modname = String.uncapitalize modname ^ ".mli"

let module_of_filename filename = String.capitalize (Filename.chop_extension filename)

(* return the (modules list) dependency for a specific file *)
let runOcamldep dopt modname =
    let srcFile = (default "" dopt.dep_src_dir) </> filename_of_module modname in
    let args = ["ocamldep" ^ (if dopt.dep_withopt then ".opt" else "")]
             @ (maybe [] (fun x -> ["-I"; x]) dopt.dep_src_dir)
             @ ["-modules"; srcFile ] in
    match run_with_outputs args with
    | Success (f,_) -> (
        match Utils.toKV f with
        | (_,None)   -> raise (DependencyAnalyzeFailed ("assumption failed: " ^ f))
        | (_,Some v) -> let vStripped = string_stripSpaces v in
                        if vStripped = ""
                               then []
                               else List.map string_stripSpaces (string_words vStripped)
        )
    | Failure er -> raise (DependencyAnalyzeFailed er)

let runOcamlCompile copt modname =
    let srcFile = (default "" copt.compile_src_dir) </> filename_of_module modname in
    let dstFile = (default "" copt.compile_dest_dir) </>
                  (if copt.compile_native then cmx_of_module else cmo_of_module) modname
        in
    let args = [ (if copt.compile_native then "ocamlopt" else "ocamlc") ^
                 (if copt.compile_withopt then ".opt" else "") ]
             @ (if copt.compile_use_thread then ["-thread"] else [])
             @ (Utils.to_include_path_options copt.compile_include_paths)
             @ ["-o"; dstFile]
             @ ["-c"; srcFile ]
        in
    match run_with_outputs args with
    | Success (_, warnings) -> warnings
    | Failure er            -> raise (CompilationFailed er)

(*
*)

let runOcamlLinking lopt libs modules =
    let args = [ (if lopt.linking_native then "ocamlopt" else "ocamlc") ^
                 (if lopt.linking_withopt then ".opt" else "") ]
             @ (if lopt.linking_use_thread then ["-thread"] else [])
             @ [ "-o"; lopt.linking_dest ]
             @ (Utils.to_include_path_options lopt.linking_include_paths)
             @ (List.map (if lopt.linking_native then cmxa_of_lib else cma_of_lib) libs)
             @ (List.map (if lopt.linking_native then cmx_of_module else cmo_of_module) modules)
             in
    match run_with_outputs args with
    | Success (_, warnings) -> warnings
    | Failure er            -> raise (LinkingFailed er)

type compilation_state =
    { compilation_modules  : (modname, module_desc) Hashtbl.t
    ; compilation_order    : modname list list
    ; compilation_builddir : string
    }

(* prepare modules dependencies and various compilation state
 * that is going to be required for compilation and linking.
 *)
let prepare generalConf buildDir srcDir toplevelModules =
    let modulesDeps = Hashtbl.create 64 in
    let rec loop modname =
        if Hashtbl.mem modulesDeps modname
        then ()
        else (
            verbose generalConf Verbose "Analysing %s\n%!" modname;
            let filepath = maybe "" (fun x -> x) srcDir </> filename_of_module modname in
            let modTime = Filesystem.getModificationTime filepath in
            verbose generalConf Debug "  %s has mtime %f\n%!" modname modTime;

            let dopt = { dep_withopt = true
                       ; dep_src_dir = srcDir
                       } in
            let allDeps = runOcamldep dopt modname in
            verbose generalConf Debug "  %s depends on %s\n%!" modname (String.concat "," allDeps);
            let (cwdDeps, otherDeps) = List.partition (fun dep ->
                let filename = maybe "" (fun x -> x) srcDir </> filename_of_module dep in
                Sys.file_exists filename
                ) allDeps in
            let hasInterface = Sys.file_exists (interface_of_module modname) in
            let use_threads = List.mem "Thread" otherDeps in
            Hashtbl.add modulesDeps modname { module_use_threads   = use_threads
                                            ; module_has_interface = hasInterface
                                            ; module_src_mtime     = modTime
                                            ; dep_cwd_modules      = cwdDeps
                                            ; dep_other_modules    = otherDeps
                                            };
            List.iter loop cwdDeps
        )
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

    { compilation_modules  = modulesDeps
    ; compilation_order    = get_order ()
    ; compilation_builddir = buildDir
    }

let check_compile_needed copt modules_deps modname =
    (*
    let dstFileO =
        if copt.compile_native
            then Some (default "" copt.compile_dest_dir </> o_of_module modname)
            else None
        in
    let dstFileCMI = (default "" copt.compile_dest_dir) </> cmi_of_module modname in
    *)
    let module_desc = Hashtbl.find modules_deps modname in

    let srcMTime = module_desc.module_src_mtime in
    (* cmx/cmo file *)
    let dstFile = (default "" copt.compile_dest_dir) </>
                  (if copt.compile_native then cmx_of_module else cmo_of_module) modname in
    let dstExists = Sys.file_exists dstFile in
    let dstMTime = Filesystem.getModificationTime dstFile in

    let deps = module_desc.dep_cwd_modules in
    let depsWithMTime = List.map (fun dep -> (dep, (Hashtbl.find modules_deps dep).module_src_mtime)) deps in

    let rec loopAbort l =
        match l with
        | []        -> None
        | (f,r)::xs -> let failed = f () in if failed then Some r else loopAbort xs
        in

    let checks =
        [ (fun () -> not dstExists), ""
        ; (fun () -> srcMTime >= dstMTime), "destination obsolete"
        ] @
        List.map (fun (dep, mt) -> ((fun () -> dstMTime < mt), dep ^ " changed")) depsWithMTime
        in
    loopAbort checks

let compile generalConf compilationPaths srcDir cstate =
    let compiledModules = ref [] in
    let modules = List.concat cstate.compilation_order in
    verbose generalConf Debug "Compiling order : %s\n%!" (String.concat "," modules);
    let nbStep = List.length modules in
    list_iteri (fun currentStep m ->
        let useThreads = (Hashtbl.find cstate.compilation_modules m).module_use_threads in
        let copt = { compile_native        = true
                   ; compile_withopt       = true
                   ; compile_dest_dir      = Some cstate.compilation_builddir
                   ; compile_src_dir       = srcDir
                   ; compile_use_thread    = useThreads
                   ; compile_include_paths = compilationPaths
                   } in

        (match check_compile_needed copt cstate.compilation_modules m with
        | Some reason -> (
                verbose generalConf Report "[%d of %d] Compiling %s%s\n%!" currentStep nbStep m
                    (if reason <> "" then "    ( " ^ reason ^ " )" else "");

                let warnings = runOcamlCompile copt m in
                if warnings <> "" then fprintf stderr "%s%!" warnings
                )
        | None ->
                ()
        );
        compiledModules := m :: !compiledModules
    ) modules;
    List.rev !compiledModules

(* compile and link a library
 *)
let compileLib generalConf projFile lib =
    verbose generalConf Report "Building library %s\n" (projFile.obuild_name);

    let buildDir = Dist.createBuildDest (Dist.Library) in

    let cstate = prepare generalConf buildDir lib.lib_srcdir lib.lib_modules in
    ignore cstate;
    ()

(* compile and link an executable
 *
 * most of the preparation is done in prepare,
 * here we compile every modules and link the result at the end.
 *
 *)
let compileExe generalConf projFile exe =
    verbose generalConf Report "Building executable %s\n" (exe.exe_name);

    let buildDir = Dist.createBuildDest (Dist.Executable exe.exe_name) in
    let srcDir =
        match exe.exe_srcdir with
        | None     -> []
        | Some dir -> [dir]
        in
    let compilationPaths = [buildDir] @ srcDir in
    let linking cstate compiled =
        let dest = cstate.compilation_builddir </> exe.exe_name in
        verbose generalConf Report "Linking %s\n%!" dest;
        let useThreadLib = List.mem "threads" exe.exe_builddeps in
        let useThreads = Hashtbl.fold (fun _ v a -> v.module_use_threads || a) cstate.compilation_modules useThreadLib in
        let lopts = { linking_native        = true
                    ; linking_withopt       = true
                    ; linking_dest          = dest ^ (if Utils.is_windows then ".exe" else "")
                    ; linking_use_thread    = useThreads
                    ; linking_include_paths = [cstate.compilation_builddir]
                    } in
        let warnings = runOcamlLinking lopts exe.exe_builddeps compiled in
        if warnings <> "" then fprintf stderr "%s%!" warnings;
        ()
    in
    (* get modules dependencies *)
    let cstate   = prepare generalConf buildDir exe.exe_srcdir [module_of_filename exe.exe_main] in
    let compiled = compile generalConf compilationPaths exe.exe_srcdir cstate in
    linking cstate compiled
