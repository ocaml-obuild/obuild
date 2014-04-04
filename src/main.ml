open Printf
open Ext.Fugue
open Ext.Filepath
open Ext
open Obuild.Types
open Obuild.Helper
open Obuild.Gconf
open Obuild

let programName = "obuild"
let usageStr cmd = "\nusage: " ^ programName ^ " " ^ cmd ^ " <options>\n\noptions:\n"

let project_read () =
    try Project.read gconf.conf_strict
    with exn -> verbose Verbose "exception during project read: %s\n" (Printexc.to_string exn); raise exn

let mainConfigure argv =
    let userFlagSettings = ref [] in
    let userSetFlagSettings s =
        let tweak =
            if string_startswith "-" s
                then ClearFlag (string_drop 1 s)
                else SetFlag s
            in
        userFlagSettings := tweak :: !userFlagSettings
        in

    let enable_disable_opt opt_name f doc =
        [ ("--enable-" ^ opt_name, Arg.Unit (f true), " enable " ^ doc)
        ; ("--disable-" ^ opt_name, Arg.Unit (f false), "disable " ^ doc)
        ]
        in

    let opts =
        [ ("--flag", Arg.String userSetFlagSettings, "enable or disable a project's flag")
        ; ("--executable-as-obj", Arg.Unit (Configure.set_exe_as_obj true), "output executable as obj file")
        ; ("--annot", Arg.Unit (Configure.set_annot true), "generate .annot files")
        ]
        in
    Arg.parse_argv (Array.of_list argv)
        ( enable_disable_opt "library-bytecode" Configure.set_lib_bytecode "library compilation as bytecode"
        @ enable_disable_opt "library-native" Configure.set_lib_native "library compilation as native"
        @ enable_disable_opt "executable-bytecode" Configure.set_exe_bytecode "executable compilation as bytecode"
        @ enable_disable_opt "executable-native" Configure.set_exe_native "executable compilation as native"
        @ enable_disable_opt "library-profiling" Configure.set_lib_profiling "library profiling"
        @ enable_disable_opt "library-debugging" Configure.set_lib_debugging "library debugging"
        @ enable_disable_opt "executable-profiling" Configure.set_exe_profiling "executable profiling"
        @ enable_disable_opt "executable-debugging" Configure.set_exe_debugging "executable debugging"
        @ enable_disable_opt "examples" Configure.set_build_examples "building examples"
        @ enable_disable_opt "benchs" Configure.set_build_benchs "building benchs"
        @ enable_disable_opt "tests" Configure.set_build_tests "building tests"
        @ opts
        ) (fun s -> failwith ("unknown option: " ^ s))
        (usageStr "configure");

    FindlibConf.load ();
    let projFile = Project.read gconf.conf_strict in
    verbose Report "Configuring %s-%s...\n" projFile.Project.name projFile.Project.version;
    Configure.run projFile !userFlagSettings;
    (* check build deps of everything buildables *)
    ()

let mainBuild argv =
    let anon = ref [] in
    let build_options =
      [ ("-j", Arg.Int (fun i -> gconf.conf_parallel_jobs <- i), "maximum number of jobs in parallel")
      ; ("--jobs", Arg.Int (fun i -> gconf.conf_parallel_jobs <- i), "maximum number of jobs in parallel")
      ; ("--dot", Arg.Unit (fun () -> gconf.conf_dump_dot <- true), "dump dependencies dot files during build")
      ; ("--noocamlmklib", Arg.Unit (fun () -> gconf.conf_ocamlmklib <- false), "do not use ocamlmklib when linking C code")
      ] in

    Arg.parse_argv (Array.of_list argv) build_options (fun s -> anon := s :: !anon) (usageStr "build");

    Configure.check ();
    let projFile = project_read () in
    FindlibConf.load ();
    let project = Analyze.prepare projFile in
    let bstate = Prepare.init project in

    let dag = match !anon with
              | [] -> project.Analyze.project_targets_dag
              | _  ->
                      let targets = List.map name_of_string !anon in
                      Dag.subset project.Analyze.project_targets_dag targets
              in
    let taskdep = Taskdep.init dag in
    while not (Taskdep.isComplete taskdep) do
        (match Taskdep.getnext taskdep with
        | None -> failwith "no free task in targets"
        | Some (step,ntask) ->
            verbose Verbose "building target %s\n%!" (name_to_string ntask);
            (match ntask with
            | ExeName name   -> Build.buildExe bstate (Project.find_exe projFile name)
            | LibName name   -> Build.buildLib bstate (Project.find_lib projFile name)
            | BenchName name -> Build.buildBench bstate (Project.find_bench projFile name)
            | TestName name  -> Build.buildTest bstate (Project.find_test projFile name)
            | ExampleName name -> Build.buildExample bstate (Project.find_example projFile name)
            );
            Taskdep.markDone taskdep ntask
        )
    done;
    ()

let mainClean argv =
    if Filesystem.exists (Dist.getDistPath ())
        then Filesystem.removeDir (Dist.getDistPath ())
        else ()

let mainSdist argv =
    let isSnapshot = ref false in
    Arg.parse_argv (Array.of_list argv)
           [ ("--snapshot", Arg.Set isSnapshot, "build a snapshot of the project")
           ] (fun s -> failwith ("unknown option: " ^ s))
           (usageStr "sdist");
    Dist.check (fun () -> ());

    let projFile = project_read () in
    Sdist.run projFile !isSnapshot;
    ()

let unimplemented () =
    eprintf "sorry, you've reached an unimplemented part ! please be patient or send a patch.\n";
    exit 1

let mainDoc argv =
    Arg.parse_argv (Array.of_list argv)
           [
           ] (fun s -> failwith ("unknown option: " ^ s))
           (usageStr "doc");

    let projFile = project_read () in
    Doc.run projFile;
    unimplemented ()

let mainInfer argv =
    let anon = ref [] in
    Arg.parse_argv (Array.of_list argv)
            [
            ] (fun s -> anon := s :: !anon)
            (usageStr "infer");

    if !anon = []
    then (printf "no modules to infer\n"; exit 0);

    unimplemented ()

let mainInstall argv =
  let dest_dir = ref "" in
  let opam_install = ref false in
  Arg.parse_argv (Array.of_list argv) [
    ("--destdir", Arg.Set_string dest_dir, "override destination where to install (default coming from findlib configuration)");
    ("--opam", Arg.Set opam_install, "only create the .install file for opam (do not copy the files)")
  ] (fun s -> failwith ("unknown option: " ^ s))
    (usageStr "install");

  Configure.check ();
  let proj_file = project_read () in
  FindlibConf.load ();
  let dest_dir =
    (if !dest_dir = ""
     then (match FindlibConf.get_destdir () with
         | None   -> failwith "no destdir specified, and no findlib default found"
         | Some p -> p
       )
     else fp !dest_dir)
  in
  (* install all the libs *)
  Install.install_libs proj_file dest_dir !opam_install;
  if !opam_install then
    Install.opam_install_file proj_file

let mainTest argv =
    let showTest = ref false in
    Arg.parse_argv (Array.of_list argv)
           [ ("--output", Arg.Set showTest, "show test outputs")
           ] (fun s -> failwith ("unknown option: " ^ s))
           (usageStr "test");

    Configure.check ();
    let projFile = project_read () in
    if not gconf.conf_build_tests then (
        eprintf "error: building tests are disabled, re-configure with --enable-tests\n";
        exit 1
    );
    let testTargets = List.map Project.test_to_target projFile.Project.tests in
    if testTargets <> []
        then (
            let results =
                List.map (fun test ->
                    let testTarget = Project.test_to_target test in
                    let outputName = Utils.to_exe_name Normal Native (Target.get_target_dest_name testTarget) in
                    let dir = Dist.getBuildDest (Dist.Target testTarget.Target.target_name) in
                    let exePath = dir </> outputName in
                    if not (Filesystem.exists exePath) then (
                        eprintf "error: %s doesn't appears built, make sure build has been run first\n" (Target.get_target_name testTarget);
                        exit 1
                    );
                    (match Process.run_with_outputs [ fp_to_string exePath ] with
                    | Process.Success (out,_) ->
                        if !showTest then print_warnings out;
                        (test.Project.test_name, true)
                    | Process.Failure err ->
                        print_warnings err;
                        (test.Project.test_name, false)
                    )
                ) projFile.Project.tests
                in
            (* this is just a mockup. expect results displayed in javascript and 3d at some point *)
            let failed = List.filter (fun (_,x) -> false = x) results in
            let successes = List.filter (fun (_,x) -> true = x) results in
            let total = List.length failed + List.length successes in
            printf "%sSUCCESS%s: %d/%d\n" (color_green()) (color_white()) (List.length successes) total;
            printf "%sFAILED%s : %d/%d\n" (color_red()) (color_white()) (List.length failed) total;
            List.iter (fun (n,_) -> printf "  %s\n" n) failed;
            if failed <> [] then exit 1

        ) else
            printf "warning: no tests defined: not doing anything.\n"

let mainGet argv =
    let argv = List.tl argv in
    let projFile = project_read () in

    (* TODO: hardcoded just for now to get basic fields.
     * - add option for quoting
     * - optional formating options for multi values (one per line, csv)
     * - access more complicated fields lib/sublib modules/dependencies, etc
     * *)
    match argv with
    | []      -> eprintf "usage: obuild get <field>\n\n"; exit 1
    | [field] -> (match field with
                 | "name"    -> printf "%s\n" projFile.Project.name;
                 | "version" -> printf "%s\n" projFile.Project.version;
                 | "license" -> printf "%s\n" projFile.Project.license;
                 | _         -> eprintf "error: unknown field %s\n" field; exit 1
                 )
    | _       -> eprintf "usage: obuild get <field>\n"; exit 1

let mainInit argv =
    let project = Init.run () in
    let name = fn (project.Project.name) <.> "obuild" in
    Project.write (in_current_dir name) project

let usageCommands = String.concat "\n"
    [ "Commands:"
    ; ""
    ; "  configure    Prepare to build the package."
    ; "  build        Make this package ready for installation."
    ; "  clean        Clean up after a build."
    ; "  sdist        Generate a source distribution file (.tar.gz)."
    ; "  doc          Generate documentation."
    ; "  install      Install this package."
    ; "  test         Run the tests"
    ; "  help         Help about commands"
    ]

let mainHelp argv =
    match argv with
    | []         -> eprintf "usage: obuild help <command>\n\n";
    | command::_ ->
        try
            let msgs = List.assoc command Help.helpMessages in
            List.iter (eprintf "%s\n") msgs
        with Not_found ->
            eprintf "no helpful documentation for %s\n" command

(* parse the global args up the first non option <command>
 * <exe> -opt1 -opt2 <command> <...>
 * *)
let parseGlobalArgs () =
    let printVersion () =
      printf "obuild %s\n" Path_generated.project_version;
      exit 0
    in
    let printHelp () = printf "a rescue team has been dispatched\n";
                       exit 0
        in
    let expect_param1 optName l f =
        match l with
        | []    -> failwith (optName ^ " expect a parameter")
        | x::xs -> f x; xs
        in
    let rec processGlobalArgs l =
        match l with
        | x::xs -> if String.length x > 0 && x.[0] = '-'
                    then (
                        let retXs =
                            match x with
                            | "--help"    -> printHelp ()
                            | "--version" -> printVersion ()
                            | "--verbose" -> gconf.conf_verbosity <- Verbose; xs
                            | "--color"   -> gconf.conf_color <- true; xs
                            | "--debug"   -> gconf.conf_verbosity <- Debug; xs
                            | "--debug+"  -> gconf.conf_verbosity <- DebugPlus; xs
                            | "--debug-with-cmd" -> gconf.conf_verbosity <- DebugPlus; xs
                            | "--silent"  -> gconf.conf_verbosity <- Silent; xs
                            | "--strict"  -> gconf.conf_strict    <- true; xs
                            | "--findlib-conf" -> expect_param1 x xs (fun p -> gconf.conf_findlib_path <- Some p)
                            | "--ocamlopt" -> expect_param1 x xs (fun p -> gconf.conf_prog_ocamlopt <- Some p)
                            | "--ocamldep" -> expect_param1 x xs (fun p -> gconf.conf_prog_ocamldep <- Some p)
                            | "--ocamlc"   -> expect_param1 x xs (fun p -> gconf.conf_prog_ocamlc <- Some p)
                            | "--cc"       -> expect_param1 x xs (fun p -> gconf.conf_prog_cc <- Some p)
                            | "--ar"       -> expect_param1 x xs (fun p -> gconf.conf_prog_ar <- Some p)
                            | "--pkg-config"-> expect_param1 x xs (fun p -> gconf.conf_prog_pkgconfig <- Some p)
                            | "--ranlib"   -> expect_param1 x xs (fun p -> gconf.conf_prog_ranlib <- Some p)
                            | _           -> failwith ("unknown global option: " ^ x)
                            in
                         processGlobalArgs retXs
                    ) else
                         l
        | []    -> []
        in

    processGlobalArgs (List.tl (Array.to_list Sys.argv))

let knownCommands =
    [ ("configure", mainConfigure)
    ; ("build", mainBuild)
    ; ("clean", mainClean)
    ; ("sdist", mainSdist)
    ; ("install", mainInstall)
    ; ("init", mainInit)
    ; ("infer", mainInfer)
    ; ("test", mainTest)
    ; ("get", mainGet)
    ; ("doc", mainDoc)
    ; ("help", mainHelp)
    ]

let defaultMain () =
    let args = parseGlobalArgs () in

    if List.length args = 0
    then (
        eprintf "usage: %s <command> [options]\n\n%s\n" Sys.argv.(0) usageCommands;
        exit 1
    );

    let cmd = List.hd args in
    try
        let mainF = List.assoc cmd knownCommands in
        mainF args
    with Not_found ->
        eprintf "error: unknown command: %s\n\n  known commands:\n" cmd;
        List.iter (eprintf "    %s\n") (List.map fst knownCommands);
        exit 1

let () =
    try defaultMain ()
    with
    | Init.ProjectAlreadyExists -> eprintf "error: found another project file in this directory. cannot run init in an already existing project\n"; exit 12
    | Init.AbortedByUser -> eprintf "init aborted. nothing written\n"; exit 0
    | exn -> Exception.show exn
