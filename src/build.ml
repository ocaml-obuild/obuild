open Types
open Ext
open Helper
open Printf
open Filepath
open Prepare

exception CCompilationFailed of string
exception CompilationFailed of string
exception LinkingFailed of string

type common_opt =
    { common_build_native : bool (* build native or bytecode *)
    ; common_use_withopt : bool (* use the .opt compiler *)
    }

type compile_opt =
    { compile_common        : common_opt
    ; compile_dest_dir      : string option
    ; compile_src_dir       : string option
    ; compile_use_thread    : bool
    ; compile_include_paths : string list
    }

type ccompile_opt =
    { ccompile_common        : common_opt
    ; ccompile_opts          : string list
    ; ccompile_include_paths : string list
    ; ccompile_dest_dir      : string option
    ; ccompile_src_dir       : string option
    }

type clinking_opt =
    { clinking_common        : common_opt
    ; clinking_shared        : bool
    ; clinking_dest          : filepath
    }

type linking_opts =
    { linking_common        : common_opt
    ; linking_lib           : bool
    ; linking_dest          : filepath
    ; linking_use_thread    : bool
    ; linking_cclibs        : string list
    ; linking_include_paths : string list
    }

let runOcamlCompile gconf copt compileInterface modname =
    let commonOpts = copt.compile_common in
    let (srcFile, dstFile) =
        if compileInterface
            then
                (with_optpath copt.compile_src_dir (interface_of_module modname)
                ,with_optpath copt.compile_dest_dir (cmi_of_module modname)
                )
            else 
                (with_optpath copt.compile_src_dir (filename_of_module modname)
                ,with_optpath copt.compile_dest_dir ((if commonOpts.common_build_native then cmx_of_module else cmo_of_module) modname)
                )
        in
    let args = [ (if commonOpts.common_build_native then "ocamlopt" else "ocamlc") ^
                 (if commonOpts.common_use_withopt then ".opt" else "") ]
             @ (if copt.compile_use_thread then ["-thread"] else [])
             @ (Utils.to_include_path_options copt.compile_include_paths)
             @ ["-o"; filepath_to_string dstFile ]
             @ ["-c"; filepath_to_string srcFile ]
        in
    match run_with_outputs gconf args with
    | Success (_, warnings) -> warnings
    | Failure er            -> raise (CompilationFailed er)

let runCCompile gconf copt file =
    let setup = gconf.conf_setup in

    let callCCompiler =
        try string_words (List.assoc "bytecomp_c_compiler" setup)
        with Not_found -> raise (CCompilationFailed "no bytecomp_c_compiler returned by ocaml")
        in
    let srcFile = with_optpath copt.ccompile_src_dir file in
    let dstFile = with_optpath copt.ccompile_dest_dir (wrap_filename (file.filename ^ ".o")) in (* make a .c.o file to avoid collision *)
    let args = callCCompiler
             @ copt.ccompile_opts
             @ (Utils.to_include_path_options copt.ccompile_include_paths)
             @ ["-o"; dstFile.filepath]
             @ ["-c"; srcFile.filepath]
        in
    match run_with_outputs gconf args with
    | Success (_, warnings) -> warnings
    | Failure er            -> raise (CCompilationFailed er)

let runCLinking gconf lopt depfiles =
    let args = [ "gcc" ]
             @ (if lopt.clinking_shared then [ "-shared" ] else [])
             @ ["-o"; lopt.clinking_dest.filepath ]
             @ depfiles
             in
    match run_with_outputs gconf args with
    | Success (_, warnings) -> warnings
    | Failure er            -> raise (LinkingFailed er)

let runAr gconf dest deps =
    let args = [ "ar"; "rc"; dest ] @ deps in
    match run_with_outputs gconf args with
    | Success (_, warnings) -> warnings
    | Failure er            -> raise (LinkingFailed er)

let runRanlib gconf dest =
    match run_with_outputs gconf [ "ranlib"; dest ] with
    | Success (_, warnings) -> warnings
    | Failure er            -> raise (LinkingFailed er)


let runOcamlLinking gconf lopt libs modules =
    let commonOpts = lopt.linking_common in
    let args = [ (if commonOpts.common_build_native then "ocamlopt" else "ocamlc") ^
                 (if commonOpts.common_use_withopt then ".opt" else "") ]
             @ (if lopt.linking_use_thread then ["-thread"] else [])
             @ (if lopt.linking_lib then ["-a"] else [])
             @ [ "-o"; lopt.linking_dest.filepath ]
             @ (Utils.to_include_path_options lopt.linking_include_paths)
             @ (List.map filepath_to_string libs)
             @ (List.map filename_to_string $ List.map (cmc_of_module commonOpts.common_build_native) modules)
             @ (List.concat (List.map (fun x -> [ "-cclib"; x ]) lopt.linking_cclibs))
             in
    match run_with_outputs gconf args with
    | Success (_, warnings) -> warnings
    | Failure er            -> raise (LinkingFailed er)

let check_compile_needed copt modules_deps modname =
    let module_desc = Hashtbl.find modules_deps modname in

    let srcMTime = module_desc.module_src_mtime in
    let intfMTime = module_desc.module_intf_mtime in

    let cmc_of_module = cmc_of_module copt.compile_common.common_build_native in
    (* cmx/cmo file *)
    let moduleCmcFile = with_optpath copt.compile_dest_dir (cmc_of_module modname) in
    let dstFileCMI = with_optpath copt.compile_dest_dir (cmi_of_module modname) in

    let dstExists = Filesystem.exists moduleCmcFile in
    let dstMTime = Filesystem.getModificationTime moduleCmcFile in
    let dstCMIMTime = Filesystem.getModificationTime dstFileCMI in

    let depSources = module_desc.dep_cwd_modules in
    let depDestTime = List.map (fun dep -> (dep, Filesystem.getModificationTime (with_optpath copt.compile_dest_dir (cmc_of_module dep)))) depSources in

    (*let depsWithMTime = List.map (fun dep -> (dep, (Hashtbl.find modules_deps
     * dep).module_src_mtime)) deps in*)

    let rec loopAbort l =
        match l with
        | []        -> None
        | (f,r)::xs -> let failed = f () in if failed then Some r else loopAbort xs
        in

    let checks =
        [ (fun () -> not dstExists), ""
        ; (fun () -> srcMTime >= dstMTime), "source changed"
        ; (fun () -> intfMTime >= dstCMIMTime), "interface changed"
        ] @
        List.map (fun (dep, mt) -> ((fun () -> dstMTime < mt), dep.modname ^ " changed")) depDestTime
        in
    loopAbort checks

let print_warnings warnings =
    if warnings <> "" then fprintf stderr "%s%!" warnings else ()

let separate_deps gstate =
    let internal = ref [] in
    let system = ref [] in
    Hashtbl.iter (fun k v ->
        if v.dep_type = Internal
            then internal := k :: !internal
            else system   := k :: !system
    ) gstate.data_deps;
    (!internal, !system)

let get_caml_include_path gstate =
    try List.assoc "standard_library" gstate.data_gconf.conf_setup
    with Not_found -> raise (CCompilationFailed "no standard_library returned by ocaml")

let get_dep_path gstate dep =
    let camlIncludePath = get_caml_include_path gstate in
    match (Hashtbl.find gstate.data_deps dep.dep_name).dep_type with
    | Internal -> Dist.createBuildDest (Dist.Library dep.dep_name)
    | System   -> with_path camlIncludePath (wrap_filename dep.dep_name)

let compile gstate target cstate =
    let camlIncludePath = get_caml_include_path gstate in

    let extraPaths = List.map (fun (dep,_) -> get_dep_path gstate dep) target.target_builddeps in

    let compiledModules = ref [] in
    let modules = List.concat cstate.compilation_order in
    let nbStep = List.length modules in
    let nbCStep = List.length cstate.compilation_csources in

    let comOpts = { common_build_native = true
                  ; common_use_withopt  = true
                  } in
    let compile_c () =
        list_iteri (fun currentStep cFile ->
            verbose gstate.data_gconf Report "[%d of %d] Compiling C %s\n%!" currentStep nbCStep (cFile.filename);
            let copt = { ccompile_common        = comOpts
                       ; ccompile_opts          = []
                       ; ccompile_include_paths = (maybe [] (fun x -> [x]) target.target_cdir) @ [camlIncludePath]
                       ; ccompile_dest_dir      = Some cstate.compilation_builddir.filepath
                       ; ccompile_src_dir       = target.target_cdir
                       } in
            let warnings = runCCompile gstate.data_gconf copt cFile in
            print_warnings warnings
        ) cstate.compilation_csources
        in
    let compile_ocaml () =
        list_iteri (fun currentStep m ->
            let useThreads = (Hashtbl.find cstate.compilation_modules m).module_use_threads in
            let hasIntf = (Hashtbl.find cstate.compilation_modules m).module_has_interface in
            let copt = { compile_common        = comOpts
                       ; compile_dest_dir      = Some cstate.compilation_builddir.filepath
                       ; compile_src_dir       = target.target_srcdir
                       ; compile_use_thread    = useThreads
                       ; compile_include_paths = List.map filepath_to_string (cstate.compilation_paths @ extraPaths)
                       } in

            (* put in the runqueue *)
            (match check_compile_needed copt cstate.compilation_modules m with
            | Some reason -> (
                    verbose gstate.data_gconf Report "[%d of %d] Compiling %s%s\n%!" currentStep nbStep (m.modname)
                        (if reason <> "" then "    ( " ^ reason ^ " )" else "");

                    if hasIntf then (
                        verbose gstate.data_gconf Debug "  Compiling Interface\n%!";
                        let warnings = runOcamlCompile gstate.data_gconf copt true m in
                        print_warnings warnings
                    );
                    let warnings = runOcamlCompile gstate.data_gconf copt false m in
                    print_warnings warnings
            )
            | None ->
                    ()
            );
            compiledModules := m :: !compiledModules
        ) modules
        in
    (* we could parallelize the C and OCaml compilation,
     * they are completely independant *)
    compile_c ();
    verbose gstate.data_gconf Debug "Compiling order : %s\n%!" (String.concat "," $ List.map modname_to_string modules);
    compile_ocaml ();

    List.rev !compiledModules

let linkCStuff gconf cstate clibName =
    let comOpts = { common_build_native = true
                  ; common_use_withopt  = true
                  } in
    let soFile = with_path cstate.compilation_builddir.filepath (wrap_filename ("dll" ^ clibName ^ ".so")) in
    let aFile = cstate.compilation_builddir.filepath </> ("lib" ^ clibName ^ ".a") in

    let clopts = { clinking_common = comOpts
                 ; clinking_shared = true
                 ; clinking_dest   = soFile
                 } in
    let cdepFiles = List.map (fun x -> cstate.compilation_builddir.filepath </> (x.filename ^ ".o")) cstate.compilation_csources in
    let warnings = runCLinking gconf clopts cdepFiles in
    print_warnings warnings;
    print_warnings (runAr gconf aFile cdepFiles);
    print_warnings (runRanlib gconf aFile);
    ()

(* compile and link a library
 *)
let compileLib gstate lib =
    let linklib cstate compiled =
        let dest = with_path cstate.compilation_builddir.filepath (cmxa_of_lib lib.lib_name) in

        let useThreadLib = List.mem "threads" $ List.map (fun (x,_) -> x.dep_name) lib.lib_builddeps in
        let useThreads = Hashtbl.fold (fun _ v a -> v.module_use_threads || a) cstate.compilation_modules useThreadLib in

        let comOpts = { common_build_native = true
                      ; common_use_withopt  = true
                      } in

        let cclibs = if cstate.compilation_csources <> [] then [lib.lib_name] else [] in
        let lopts = { linking_common        = comOpts
                    ; linking_lib           = true
                    ; linking_dest          = dest
                    ; linking_use_thread    = useThreads
                    ; linking_cclibs        = List.map (fun x -> "-l" ^ x) cclibs 
                    ; linking_include_paths = [cstate.compilation_builddir.filepath]
                    } in
        linkCStuff gstate.data_gconf cstate gstate.data_projFile.obuild_name;
        let warnings = runOcamlLinking gstate.data_gconf lopts [] compiled in
        print_warnings warnings;
        ()
        in

    verbose gstate.data_gconf Report "Building library %s\n" lib.lib_name;
    let buildDir = Dist.createBuildDest (Dist.Library lib.lib_name) in
    let target = { target_srcdir    = lib.lib_srcdir
                 ; target_cdir      = lib.lib_cdir
                 ; target_csources  = lib.lib_csources
                 ; target_builddeps = lib.lib_builddeps
                 } in
    let cstate = prepare_compilation gstate buildDir target lib.lib_modules in
    let compiled = compile gstate target cstate in
    verbose gstate.data_gconf Report "Linking library %s\n" lib.lib_name;
    linklib cstate compiled;
    ()

(* compile and link an executable
 *
 * most of the preparation is done in prepare,
 * here we compile every modules and link the result at the end.
 *
 *)
let compileExe gstate exe =
    verbose gstate.data_gconf Report "Building executable %s\n" (exe.exe_name);

    let buildDir = Dist.createBuildDest (Dist.Executable exe.exe_name) in

    let extraPaths = List.map (fun (dep,_) -> get_dep_path gstate dep) exe.exe_builddeps in

    let (selfDeps, systemDeps) = separate_deps gstate in
    let selfLibDirs = List.map (fun dep -> Dist.createBuildDest (Dist.Library dep)) selfDeps in

    let linking cstate compiled =
        let outputName = wrap_filename (exe.exe_name ^ (if Utils.is_windows then ".exe" else "")) in
        let dest = with_path cstate.compilation_builddir.filepath outputName in
        verbose gstate.data_gconf Report "Linking %s\n%!" dest.filepath;

        let useThreadLib =
            try let _ = List.find (fun (dep,_) -> dep.dep_name = "threads") exe.exe_builddeps in true
            with _ -> false
            in
        let useThreads = Hashtbl.fold (fun _ v a -> v.module_use_threads || a) cstate.compilation_modules useThreadLib in
        let comOpts = { common_build_native = true
                      ; common_use_withopt  = true
                      } in
        let lopts = { linking_common        = comOpts
                    ; linking_lib           = false
                    ; linking_dest          = dest
                    ; linking_cclibs        = List.map (fun x -> "-L" ^ x.filepath) selfLibDirs
                    ; linking_use_thread    = useThreads
                    ; linking_include_paths = List.map filepath_to_string ([cstate.compilation_builddir] @ extraPaths)
                    } in

        List.iter (fun (dep,_) ->
            verbose gstate.data_gconf Report "Depending on %s(%s)\n%!" dep.dep_name  (String.concat "." dep.dep_subname)
        ) exe.exe_builddeps;

        let buildDeps = (*List.map (fun x -> with_path "" (cmxa_of_lib x)) $ *)
            List.map (fun (dep,_) ->
                let path = get_dep_path gstate dep in
                with_path "" (cmxa_of_lib dep.dep_name)
            ) exe.exe_builddeps
            in
        (*
        let extraSystemDeps = List.map (fun sysdep ->
            match (Hashtbl.find gstate.data_deps sysdep).dep_meta with
            | None      -> failwith ("internal error: system library without a meta " ^ sysdep)
            | Some meta -> (*Meta.find meta*)
        ) systemDeps in
                      (*@ (List.map (fun libDir -> with_path libDir (cmxa_of_lib
                       * gstate.data_projFile.obuild_name)) libDirs)*)
                      in
                      *)
        let warnings = runOcamlLinking gstate.data_gconf lopts buildDeps compiled in
        print_warnings warnings;
        ()
        in
    (* get modules dependencies *)
    let target = { target_srcdir    = exe.exe_srcdir
                 ; target_cdir      = exe.exe_cdir
                 ; target_csources  = exe.exe_csources
                 ; target_builddeps = exe.exe_builddeps
                 } in
    let cstate   = prepare_compilation gstate buildDir target [module_of_filename (wrap_filename exe.exe_main)] in
    let compiled = compile gstate target cstate in
    linking cstate compiled;
    ()
