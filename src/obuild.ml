open Printf
open Ext
open Types
open Helper
open Conf

let major = 0
let minor = 0

let mainConfigure generalConf argv =
    let usageConfigure = "configure" in
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
                    usageConfigure;

    let projFile = projectRead () in
    verbose generalConf Report "Configuring %s-%s...\n" projFile.obuild_name projFile.obuild_version;
    Configure.run generalConf projFile;
    (* check build deps of everything buildables *)
    ()

let mainBuild generalConf argv =
    let projFile = projectRead () in
    Configure.check generalConf;

    List.iter (Build.compileLib generalConf projFile) projFile.obuild_libs;
    List.iter (Build.compileExe generalConf projFile) projFile.obuild_exes;
    ()

let mainClean _ argv =
    if Sys.file_exists Dist.distPath
        then Filesystem.removeDir Dist.distPath
        else ()

let mainSdist _ argv =
    let usageSdist = "sdist" in

    let isSnapshot = ref false in
    Arg.parse_argv (Array.of_list argv)
           [ ("--snapshot", Arg.Set isSnapshot, "build a snapshot of the project")
           ] (fun s -> failwith ("unknown option: " ^ s))
           usageSdist;
    Dist.check (fun () -> ());

    let projFile = projectRead () in
    let name = projFile.obuild_name in
    let ver = projFile.obuild_version in
    let sdistDir = name ^ "-" ^ ver in
    let sdistName = sdistDir ^ ".tar.gz" in
    let dest = Dist.distPath </> sdistDir in

    exit 0;
    (*
    Dist.with_temporary_dir (fun dir ->
        Unix.chdir dir
        let 
    );
    *)

    Unix.mkdir dest 0o755;
    (*List.iter (fun x -> ()) obuild.obuild_lib *)

    Unix.chdir dest;
    Prog.runTar sdistName sdistDir;
    ()

let mainHelp _ argv =
    ()

let knownCommands = [ "configure"; "build"; "clean"; "sdist"; "help" ]
let usageCommands = String.concat "\n"
    [ "Commands:"
    ; "  configure    Prepare to build the package."
    ; "  build        Make this package ready for installation."
    ; "  clean        Clean up after a build."
    ; "  sdist        Generate a source distribution file (.tar.gz)."
    ; "  help         Help about commands"
    ]

let parseGlobalArgs () =
    let printVersion () = printf "obuild %d.%d\n" major minor; exit 0
        in
    let printHelp () = printf "a rescue team has been dispatched\n";
                       exit 0
        in
    let rec processGlobalArgs conf l =
        match l with
        | x::xs -> if String.length x > 0 && x.[0] = '-'
                    then let nconf =
                            match x with
                            | "--help"    -> printHelp ()
                            | "--version" -> printVersion ()
                            | "--verbose" -> { conf with conf_verbosity = Verbose }
                            | "--debug"   -> { conf with conf_verbosity = Debug }
                            | "--silent"  -> { conf with conf_verbosity = Silent }
                            | _           -> failwith ("unknown global option: " ^ x)
                            in
                         processGlobalArgs nconf xs
                    else (conf, l)
        | []    -> (conf, [])
        in
    let generalConf = getGeneralConfig () in
    processGlobalArgs generalConf (List.tl (Array.to_list Sys.argv))

let defaultMain () =
    let (generalConf, args) = parseGlobalArgs () in

    if List.length args = 0
    then (
        eprintf "usage: %s <command> [options]\n\n%s\n" Sys.argv.(0) usageCommands;
        exit 1
    );

    match List.nth args 0 with
    | "configure" -> mainConfigure generalConf args
    | "build"     -> mainBuild generalConf args
    | "clean"     -> mainClean generalConf args
    | "sdist"     -> mainSdist generalConf args
    | "help"      -> mainHelp generalConf args
    | cmd         -> eprintf "error: unknown command: %s\n\n  known commands: " cmd;
                     List.iter (eprintf "    %s\n") knownCommands;
                     exit 1

let () =
    try defaultMain ()
    with
        (* project file related *)
        | NoConfFile        -> eprintf "error: couldn't find obuild file\n"; exit 3
        | MultipleConfFiles -> eprintf "error: multiples obuild files found\n"; exit 3
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
        | Build.CompilationFailed e       -> eprintf "\n%s" e; exit 5
        | Build.DependencyAnalyzeFailed e -> eprintf "\n%s" e; exit 6
        | Build.LinkingFailed e           -> eprintf "\n%s" e; exit 7
        (* others exception *)
        | Exit              -> ()
        | e                 -> eprintf "uncaught exception\n"; raise e
