open Printf
open Ext
open Types
open Helper
open Filepath
open Gconf

let major = 0
let minor = 0

let programName = "obuild"
let usageStr cmd = "\nusage: " ^ programName ^ " " ^ cmd ^ " <options>\n\noptions:\n"

let mainConfigure argv =
    let useBytecode = ref false in
    let useNative   = ref false in
    let userFlagSettings = ref [] in
    let userSetFlagSettings s =
        let tweak =
            if String.length s > 0 && s.[0] = '-'
                then ClearFlag (string_drop 1 s)
                else SetFlag s
            in
        userFlagSettings := tweak :: !userFlagSettings
        in
    Arg.parse_argv (Array.of_list argv)
        [ ("--enable-bytecode", Arg.Set useBytecode, "enable compilation as bytecode")
        ; ("--enable-native", Arg.Set useNative, "enable compilation as native")
        ; ("--disable-bytecode", Arg.Clear useBytecode, "disable compilation as bytecode")
        ; ("--disable-native", Arg.Clear useNative, "disable compilation as native")
        ; ("--flag", Arg.String userSetFlagSettings, "enable or disable a project's flag")
        ] (fun s -> failwith ("unknown option: " ^ s))
        (usageStr "configure");

    let projFile = Project.read gconf.conf_strict in
    verbose Report "Configuring %s-%s...\n" projFile.Project.name projFile.Project.version;
    Configure.run projFile !userFlagSettings;
    (* check build deps of everything buildables *)
    ()

let mainBuild argv =
    let projFile = Project.read gconf.conf_strict in

    Configure.check ();

    let jopt = ref None in
    let dotopt = ref false in
    Arg.parse_argv (Array.of_list argv)
        [ ("-j", Arg.Int (fun i -> jopt := Some i), "maximum number of jobs in parallel")
        ; ("--dot", Arg.Set dotopt, "dump dependencies dot files during build")
        ] (fun s -> failwith ("unknown option: " ^ s))
        (usageStr "build");

    let build_opts = { Prepare.opt_nb_jobs_par = default 2 !jopt
                     ; Prepare.opt_dump_dot    = !dotopt
                     }
        in
    let project = Prepare.prepare projFile build_opts in
    let bstate = Building.init project in
    List.iter (Build.buildLib bstate) projFile.Project.libs;
    List.iter (Build.buildExe bstate) projFile.Project.exes;
    ()

let mainClean argv =
    if Filesystem.exists Dist.distPath
        then Filesystem.removeDir Dist.distPath
        else ()

let mainSdist argv =
    let isSnapshot = ref false in
    Arg.parse_argv (Array.of_list argv)
           [ ("--snapshot", Arg.Set isSnapshot, "build a snapshot of the project")
           ] (fun s -> failwith ("unknown option: " ^ s))
           (usageStr "sdist");
    Dist.check (fun () -> ());

    let projFile = Project.read gconf.conf_strict in
    let name = projFile.Project.name in
    let ver = projFile.Project.version in
    let sdistDir = name ^ "-" ^ ver in
    let sdistName = sdistDir ^ ".tar.gz" in
    ignore sdistName;

    (*let dest = Dist.distPath </> sdistDir in*)

    (*
    Dist.with_temporary_dir (fun dir ->
        Unix.chdir dir
    );
    *)

    (*
    Unix.mkdir dest 0o755;
    (*List.iter (fun x -> ()) obuild.obuild_lib *)

    Unix.chdir dest;
    Prog.runTar sdistName sdistDir;
    *)
    ()

let mainHelp argv =
    ()

let mainInstall argv =
    Dist.check (fun () -> ());
    Arg.parse_argv (Array.of_list argv)
           [
           ] (fun s -> failwith ("unknown option: " ^ s))
           (usageStr "install");
    let projFile = Project.read gconf.conf_strict in
    ignore projFile

let mainTest argv =
    Dist.check (fun () -> ());
    Arg.parse_argv (Array.of_list argv)
           [
           ] (fun s -> failwith ("unknown option: " ^ s))
           (usageStr "test");
    let projFile = Project.read gconf.conf_strict in
    ignore projFile

let knownCommands = [ "configure"; "build"; "clean"; "sdist"; "install"; "test"; "help" ]
let usageCommands = String.concat "\n"
    [ "Commands:"
    ; ""
    ; "  configure    Prepare to build the package."
    ; "  build        Make this package ready for installation."
    ; "  clean        Clean up after a build."
    ; "  sdist        Generate a source distribution file (.tar.gz)."
    ; "  install      Install this package."
    ; "  test         Run the tests"
    ; "  help         Help about commands"
    ]

(* parse the global args up the first non option <command>
 * <exe> -opt1 -opt2 <command> <...>
 * *)
let parseGlobalArgs () =
    let printVersion () = printf "obuild %d.%d\n" major minor; exit 0
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
                            | "--debug"   -> gconf.conf_verbosity <- Debug; xs
                            | "--debug-with-cmd" -> gconf.conf_verbosity <- DebugPlus; xs
                            | "--silent"  -> gconf.conf_verbosity <- Silent; xs
                            | "--strict"  -> gconf.conf_strict    <- true; xs
                            | "--ocamlopt" -> expect_param1 x xs (fun p -> gconf.conf_prog_ocamlopt <- Some p)
                            | "--ocamldep" -> expect_param1 x xs (fun p -> gconf.conf_prog_ocamldep <- Some p)
                            | "--ocamlc"   -> expect_param1 x xs (fun p -> gconf.conf_prog_ocamlc <- Some p)
                            | "--cc"       -> expect_param1 x xs (fun p -> gconf.conf_prog_cc <- Some p)
                            | "--ar"       -> expect_param1 x xs (fun p -> gconf.conf_prog_ar <- Some p)
                            | "--ranlib"   -> expect_param1 x xs (fun p -> gconf.conf_prog_ranlib <- Some p)
                            | _           -> failwith ("unknown global option: " ^ x)
                            in
                         processGlobalArgs retXs
                    ) else
                         l
        | []    -> []
        in

    processGlobalArgs (List.tl (Array.to_list Sys.argv))

let defaultMain () =
    let args = parseGlobalArgs () in

    if List.length args = 0
    then (
        eprintf "usage: %s <command> [options]\n\n%s\n" Sys.argv.(0) usageCommands;
        exit 1
    );

    match List.nth args 0 with
    | "configure" -> mainConfigure args
    | "build"     -> mainBuild args
    | "clean"     -> mainClean args
    | "sdist"     -> mainSdist args
    | "install"   -> mainInstall args
    | "test"      -> mainTest args
    | "help"      -> mainHelp args
    | cmd         -> eprintf "error: unknown command: %s\n\n  known commands:\n" cmd;
                     List.iter (eprintf "    %s\n") knownCommands;
                     exit 1

let () =
    try defaultMain ()
    with
        | Arg.Bad err       -> eprintf "%s\n" err; exit 2
        (* project file related *)
        | Project.NoConfFile        -> eprintf "error: couldn't find obuild file\n"; exit 3
        | Project.MultipleConfFiles -> eprintf "error: multiples obuild files found\n"; exit 3
        | Project.SublibNotCorrect (sn,ms,cs) ->
                eprintf "error: sublib %s referencing unknown files\n" sn;
                (if ms <> [] then eprintf "    modules : %s\n" (Utils.showList "," Modname.modname_to_string ms));
                (if cs <> [] then eprintf "    csources: %s\n" (Utils.showList "," fn_to_string cs));
                exit 3;
        (* dist directory related *)
        | Dist.NotADirectory -> eprintf "error: dist is not a directory\n"; exit 4
        | Dist.DoesntExist   -> eprintf "error: run the configure command first\n"; exit 4
        (* reconfigure *)
        | Configure.ConfigChanged r ->
                (match r with
                | "digest" -> eprintf "error: project file changed. run the configure command again\n"; exit 4
                | _        -> eprintf "error: config changed (reason=%s). run the configure command again\n" r; exit 4
                )
        (* build related failure *)
        | Building.ModuleDependsItself m  -> eprintf "error: cyclic dependency module detected in module %s\n" m.Modname.modname; exit 5
        | Build.CompilationFailed e       -> eprintf "\n%s\n%!" e; exit 6
        | Prepare.BuildDepAnalyzeFailed e -> eprintf "\n%s" e; exit 7
        | Build.LinkingFailed e           -> eprintf "\n%s\n%!" e; exit 8
        (* others exception *)
        | Exit              -> ()
        | e                 -> eprintf "uncaught exception\n"; raise e
