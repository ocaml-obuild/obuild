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

let read_setup () =
  FindlibConf.load ();
  let setup = Dist.read_setup () in
  (* all_options are restored from setup file *)
  Configure.set_opts setup;
  setup

let project_read () =
  try Project.read gconf.strict
  with exn -> verbose Verbose "exception during project read: %s\n" (Printexc.to_string exn);
    raise exn

let configure argv =
  let user_flags = ref [] in
  let user_opts = ref [] in
  let user_set_flags s =
    let tweak = if string_startswith "-" s then Configure.ClearFlag (string_drop 1 s) else Configure.SetFlag s
    in
    user_flags := tweak :: !user_flags
  in
  let set_target_options field value () =
    let opt_name = if (List.mem field ["examples"; "benchs"; "tests"]) then ("build-" ^ field) else field in
    user_opts := (opt_name,value) :: !user_opts
  in
  let enable_disable_opt opt_name doc = [
    ("--enable-" ^ opt_name, Arg.Unit (set_target_options opt_name true), " enable " ^ doc);
    ("--disable-" ^ opt_name, Arg.Unit (set_target_options opt_name false), "disable " ^ doc)
  ] in
  let opts = [
    ("--flag", Arg.String user_set_flags, "enable or disable a project's flag");
    ("--executable-as-obj", Arg.Unit (set_target_options "executable-as-obj" true), "output executable as obj file");
    ("--annot", Arg.Unit (set_target_options "annot" true), "generate .annot files");
    ("-g", Arg.Unit (fun () ->
        (set_target_options "library-debugging" true)();
        (set_target_options "executable-debugging" true)();
    ), "compilation with debugging");
    ("-pg", Arg.Unit (fun () ->
        (set_target_options "library-profiling" true)();
        (set_target_options "executable-profiling" true)();
    ), "compilation with profiling")
  ] in
  Arg.parse_argv (Array.of_list argv) (
    enable_disable_opt "library-bytecode" "library compilation as bytecode"
    @ enable_disable_opt "library-native" "library compilation as native"
    @ enable_disable_opt "library-plugin" "library compilation as native plugin"
    @ enable_disable_opt "executable-bytecode" "executable compilation as bytecode"
    @ enable_disable_opt "executable-native" "executable compilation as native"
    @ enable_disable_opt "library-profiling" "library profiling"
    @ enable_disable_opt "library-debugging" "library debugging"
    @ enable_disable_opt "executable-profiling" "executable profiling"
    @ enable_disable_opt "executable-debugging" "executable debugging"
    @ enable_disable_opt "examples" "building examples"
    @ enable_disable_opt "benchs" "building benchs"
    @ enable_disable_opt "tests" "building tests"
    @ opts
  ) (fun s -> failwith ("unknown option: " ^ s)) (usageStr "configure");

  FindlibConf.load ();
  let proj_file = Project.read gconf.strict in
  verbose Report "Configuring %s-%s...\n" proj_file.Project.name proj_file.Project.version;
  Configure.run proj_file !user_flags !user_opts;
  (* check build deps of everything buildables *)
  ()

let mainBuild argv =
  let anon = ref [] in
  let build_options = [
    ("-j", Arg.Int (fun i -> gconf.parallel_jobs <- i), "maximum number of jobs in parallel");
    ("--jobs", Arg.Int (fun i -> gconf.parallel_jobs <- i), "maximum number of jobs in parallel");
    ("--dot", Arg.Unit (fun () -> gconf.dump_dot <- true), "dump dependencies dot files during build");
    ("--noocamlmklib", Arg.Unit (fun () -> gconf.ocamlmklib <- false), "do not use ocamlmklib when linking C code")
  ] in
  Arg.parse_argv (Array.of_list argv) build_options (fun s -> anon := s :: !anon) (usageStr "build");

  Dist.exist ();
  let setup = read_setup () in
  let proj_file = project_read () in
  let flags = Configure.check proj_file true setup in
  let project = Analyze.prepare proj_file flags in
  let bstate = Prepare.init project in

  let dag = match !anon with
    | [] -> project.Analyze.project_targets_dag
    | _  ->
      let targets = List.map Target.Name.of_string !anon in
      Dag.subset project.Analyze.project_targets_dag targets
  in
  Build.build_dag bstate proj_file dag

let mainClean _ =
  if Filesystem.exists (Dist.get_path ())
  then begin
    Filesystem.removeDir (Dist.get_path ());
    Dist.remove_dead_links ()
  end

let mainSdist argv =
    let isSnapshot = ref false in
    Arg.parse_argv (Array.of_list argv)
           [ ("--snapshot", Arg.Set isSnapshot, "build a snapshot of the project")
           ] (fun s -> failwith ("unknown option: " ^ s))
           (usageStr "sdist");
    Dist.check_exn (fun () -> ());

    let proj_file = project_read () in
    Sdist.run proj_file !isSnapshot;
    ()

let unimplemented () =
    eprintf "sorry, you've reached an unimplemented part ! please be patient or send a patch.\n";
    exit 1

let mainDoc argv =
    Arg.parse_argv (Array.of_list argv)
           [
           ] (fun s -> failwith ("unknown option: " ^ s))
           (usageStr "doc");

    let proj_file = project_read () in
    Doc.run proj_file;
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

  Dist.exist ();
  let setup = read_setup () in
  let proj_file = project_read () in
  let flags = Configure.check proj_file false setup in
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
    Install.opam_install_file proj_file flags

let mainTest argv =
    let showTest = ref false in
    Arg.parse_argv (Array.of_list argv)
           [ ("--output", Arg.Set showTest, "show test outputs")
           ] (fun s -> failwith ("unknown option: " ^ s))
           (usageStr "test");

    let setup = read_setup () in
    let proj_file = project_read () in
    let _ = Configure.check proj_file false setup in
    if not (Gconf.get_target_option "build-tests") then (
        eprintf "error: building tests are disabled, re-configure with --enable-tests\n";
        exit 1
    );
    let testTargets = List.map Project.Test.to_target proj_file.Project.tests in
    if testTargets <> []
        then (
            let results =
                List.map (fun test ->
                    let testTarget = Project.Test.to_target test in
                    let outputName = Utils.to_exe_name Normal Native (Target.get_target_dest_name testTarget) in
                    let dir = Dist.get_build_exn (Dist.Target testTarget.Target.target_name) in
                    let exePath = dir </> outputName in
                    if not (Filesystem.exists exePath) then (
                        eprintf "error: %s doesn't appears built, make sure 'obuild build' is run first\n" (Target.get_target_name testTarget);
                        exit 1
                    );
                    (match Process.run [ fp_to_string exePath ] with
                    | Process.Success (out,_,_) ->
                        if !showTest then print_warnings out;
                        (test.Project.Test.name, true)
                    | Process.Failure err ->
                        print_warnings err;
                        (test.Project.Test.name, false)
                    )
                ) proj_file.Project.tests
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
    let proj_file = project_read () in

    (* TODO: hardcoded just for now to get basic fields.
     * - add option for quoting
     * - optional formating options for multi values (one per line, csv)
     * - access more complicated fields lib/sublib modules/dependencies, etc
     * *)
    match argv with
    | []      -> eprintf "usage: obuild get <field>\n\n"; exit 1
    | [field] -> (match field with
                 | "name"    -> printf "%s\n" proj_file.Project.name;
                 | "version" -> printf "%s\n" proj_file.Project.version;
                 | "license" -> printf "%s\n" proj_file.Project.license;
                 | _         -> eprintf "error: unknown field %s\n" field; exit 1
                 )
    | _       -> eprintf "usage: obuild get <field>\n"; exit 1

let mainInit _ =
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
    let printVersion () = printf "obuild %s\n" Path_generated.project_version; exit 0
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
                            | "-v"
                            | "--verbose" -> gconf.verbosity <- Verbose; xs
                            | "--color"   -> gconf.color <- true; xs
                            | "-vv"
                            | "--debug"   -> gconf.verbosity <- Debug; xs
                            | "-vvv"
                            | "--debug+"
                            | "--debug-with-cmd" -> gconf.verbosity <- DebugPlus; xs
                            | "-q" (* for quiet *)
                            | "--silent"  -> gconf.verbosity <- Silent; xs
                            | "--strict"  -> gconf.strict    <- true; xs
                            | "--findlib-conf" -> expect_param1 x xs (fun p -> Gconf.set_env "findlib-path" p)
                            | "--ocamlopt" -> expect_param1 x xs (fun p -> Gconf.set_env "ocamlopt" p)
                            | "--ocamldep" -> expect_param1 x xs (fun p -> Gconf.set_env "ocamldep" p)
                            | "--ocamlc"   -> expect_param1 x xs (fun p -> Gconf.set_env "ocamlc" p)
                            | "--cc"       -> expect_param1 x xs (fun p -> Gconf.set_env "cc" p)
                            | "--ar"       -> expect_param1 x xs (fun p -> Gconf.set_env "ar" p)
                            | "--pkg-config"-> expect_param1 x xs (fun p -> Gconf.set_env "pkgconfig" p)
                            | "--ranlib"   -> expect_param1 x xs (fun p -> Gconf.set_env "ranlib" p)
                            | _           -> failwith ("unknown global option: " ^ x)
                            in
                         processGlobalArgs retXs
                    ) else
                         l
        | []    -> []
        in

    processGlobalArgs (List.tl (Array.to_list Sys.argv))

let knownCommands =
    [ ("configure", configure)
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
