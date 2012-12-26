open Printf
open Types
open Helper
open Conf

let major = 0
let minor = 0

let mainConfigure argv =
    let general_conf = getGeneralConfig () in
    let projFile = projectRead () in

    checkDistOrCreate ();
    ()

let mainBuild argv =
    let general_conf = getGeneralConfig () in
    let projFile = projectRead () in
    checkDistOrFail ();

    List.iter (Build.compileExe general_conf projFile) projFile.obuild_exes;
    ()

let mainClean argv =
    let projFile = projectRead () in
    checkDist (fun () -> ());
    Filesystem.removeDir distPath;
    ()

let mainSdist argv =
    ()

let mainHelp argv =
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

let defaultMain =
    let args =
        let printVer () = printf "obuild %d.%d\n" major minor;
                          exit 0
            in
        let printHelp () = printf "a rescue team has been dispatched\n";
                           exit 0
            in
        let argLeft = ref [] in
        Arg.parse [ ("--help", Arg.Unit printHelp, "print help")
                  ; ("--version", Arg.Unit printVer, "print version")
                  ] (fun s -> argLeft := s :: !argLeft) "usage";
        !argLeft
        in
   
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
    | "help"      -> mainHelp args
    | cmd         -> eprintf "error: unknown command: %s\n\n  known commands: " cmd;
                     List.iter (eprintf "    %s\n") knownCommands

let () =
    try defaultMain
    with
        | NoConfFile        -> eprintf "error: couldn't find obuild file\n"
        | MultipleConfFiles -> eprintf "error: multiples obuild files found\n" 
        | DistNotADirectory -> eprintf "error: dist is not a directory\n"
        | DistDoesntExist   -> eprintf "error: run the configure command first\n"
        | Build.CompilationFailed e -> eprintf "error: compilation failed:\n%s\n" e
        | Build.DependencyAnalyzeFailed e -> eprintf "error: failure to generate dependencies:\n%s\n" e
        | Exit              -> ()
        | e                 -> raise e
