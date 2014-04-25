(* simple builder *)
open Printf
open Ext.Fugue
open Ext.Filepath
open Ext
open Obuild.Project
open Obuild.Target
open Obuild.Types
open Obuild.Gconf
open Obuild

exception NoMain
exception TooManyArgs

let main () =
    let buildNative = ref true in
    let profiling = ref false in
    let debugging = ref false in
    let srcDir = ref currentDir in
    let cDir = ref currentDir in
    let depends = ref [] in
    let cpkgs = ref [] in
    let cfiles = ref [] in
    let cincludes = ref [] in
    let clibpaths = ref [] in
    let clibs = ref [] in

    let set_fp r v = r := fp v in
    let append f l v = l := f v :: !l in

    let anonParams = ref [] in
    let removeDist = ref true in
    Arg.parse
        [ ("--debug", Arg.Unit (fun () -> gconf.conf_verbosity <- DebugPlus; removeDist := false), "activate build system debug")
        ; ("--native", Arg.Set buildNative , "build native executable")
        ; ("--bytecode", Arg.Clear buildNative, "build bytecode executable")
        ; ("-p", Arg.Set profiling, "build with profiling")
        ; ("-g", Arg.Set debugging, "build with debugging")
        ; ("--srcdir", Arg.String (set_fp srcDir), "where to find the ML sources (default: current directory)")
        ; ("--cdir", Arg.String (set_fp cDir), "where to find the C sources (default: current directory)")
        ; ("--cinclude", Arg.String (append fp cincludes), "append one path to the C include files")
        ; ("--clibpath", Arg.String (append fp clibpaths), "append one path to the list of path")
        ; ("--clib", Arg.String (append id clibs), "append one system library")
        ; ("--cfile", Arg.String (append fn cfiles), "append one c file")
        ; ("--cpkg", Arg.String (append id cpkgs), "append one c pckage")
        ; ("--dep", Arg.String (append lib_name_of_string depends), "append one dependency")
        ; ("--depends", Arg.String (append lib_name_of_string depends), "append one dependency")
        ]
        (fun anon -> anonParams := anon :: !anonParams)
        "usage: obuild-simple [opts] main.ml";

    let main =
        match !anonParams with
        | [] -> raise NoMain
        | [x] -> x
        | _   -> raise TooManyArgs
        in

    Gconf.set_target_options "executable-native" !buildNative;
    Gconf.set_target_options "executable-bytecode" (not !buildNative);
    Gconf.set_target_options "executable-profiling" (!profiling);
    Gconf.set_target_options "executable-debugging" (!debugging);

    let name = Filename.chop_extension main in
    let target =
        { target_name = ExeName name
        ; target_type = Exe
        ; target_cbits =
            { target_cdir      = !cDir
            ; target_csources  = List.rev !cfiles
            ; target_cflags    = []
            ; target_clibs     = List.rev !clibs
            ; target_clibpaths = List.rev !clibpaths
            ; target_cpkgs     = List.map (fun p -> (p, None)) !cpkgs (* no constraints *)
            }
        ; target_obits =
            { target_srcdir    = !srcDir
            ; target_builddeps = List.map (fun p -> (p, None)) !depends (* no constraints *)
            ; target_oflags    = []
            ; target_pp        = None
            ; target_extradeps = []
            ; target_stdlib    = Stdlib_Standard
            }
        ; target_buildable   = BoolConst true
        ; target_installable = BoolConst true
        ; target_extras      = []
        }
        in
    let exe =
        { exe_name   = name
        ; exe_main   = fn main
        ; exe_target = target
        }
        in
    let project_config =
        { emptyObuild with
              name       = name
            ; version    = "0.0.0"
            ; obuild_ver = 1
            ; exes       = [exe]
        }
        in


    let tmpDir = Filesystem.mktemp_dir_in "dist-" in
    Dist.setDistPath tmpDir;
    try
        finally (fun () ->
            Dist.checkOrCreate ();
            let _ = Dist.createBuildDest (Dist.Autogen) in
            let buildDir = Dist.createBuildDest (Dist.Target exe.exe_target.target_name) in
            FindlibConf.load ();
            let project = Analyze.prepare project_config [] in
            let bstate = Prepare.init project in
            Build.build_exe bstate exe;
            let files = Build.get_destination_files exe.exe_target in
            List.iter (fun file ->
                printf "copying %s to %s\n" (fp_to_string (buildDir </> file)) (fp_to_string $ in_current_dir file);
                Filesystem.copy_file (buildDir </> file) (in_current_dir file)
            ) files
        ) (fun () -> if !removeDist then Filesystem.removeDir tmpDir)
    with exn -> Exception.show exn

let () =
    try main ()
    with
        | NoMain -> eprintf "error: missing main argument, expecting one ml file as parameter\n"; exit 1
        | TooManyArgs -> eprintf "too many arguments, expecting just one ml file as parameter\n"; exit 1
