(* simple builder *)
open Printf
open Ext.Fugue
open Ext.Filepath
open Ext
open Obuild.Project
open Obuild.Target
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

    let append_several
        (f: string -> Obuild.Libname.t)
        (l: Obuild.Libname.t list ref)
        (v: string): unit =
      let values = string_split ',' v in
      let values' = List.rev_map f values in
      l := List.rev_append values' !l
    in

    let anonParams = ref [] in
    let removeDist = ref true in
    Arg.parse
        [ ("--debug", Arg.Unit (fun () -> gconf.verbosity <- DebugPlus; removeDist := false), "activate build system debug")
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
        ; ("--dep", Arg.String (append Libname.of_string depends), "append one dependency")
        ; ("--deps", Arg.String (append_several Libname.of_string depends),
           "x,y,z append dependencies x, y and z")
        ; ("--depends", Arg.String (append Libname.of_string depends), "append one dependency")
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
        { target_name = Name.Exe name
        ; target_type = Typ.Exe
        ; target_cbits =
            { target_cdir      = !cDir
            ; target_csources  = List.rev !cfiles
            ; target_cflags    = []
            ; target_clibs     = List.rev !clibs
            ; target_clibpaths = List.rev !clibpaths
            ; target_cpkgs     = List.map (fun p -> (p, None)) !cpkgs (* no constraints *)
            }
        ; target_obits = {
            target_srcdir    = [!srcDir];
            target_builddeps = List.map (fun p -> (p, None)) !depends; (* no constraints *)
            target_oflags    = [];
            target_pp        = None;
            target_extradeps = [];
            target_stdlib    = Stdlib_Standard;
          }
        ; target_buildable   = BoolConst true
        ; target_installable = BoolConst true
        ; target_extras      = []
        }
        in
    let exe =
        { Executable.name   = name
        ; Executable.main   = fn main
        ; Executable.target = target
        }
        in
    let project_config =
        { Project.make with
              Project.name       = name
            ; Project.version    = "0.0.0"
            ; Project.obuild_ver = 1
            ; Project.exes       = [exe]
        }
        in


    let file_or_link_exists fn = try let _ = Unix.lstat fn in true with _ -> false in
    let tmpDir = Filesystem.mktemp_dir_in "dist-" in
    Dist.set_path tmpDir;
    try
        finally (fun () ->
            Dist.create_maybe ();
            let _ = Dist.create_build (Dist.Autogen) in
            let buildDir = Dist.create_build (Dist.Target exe.Executable.target.target_name) in
            FindlibConf.load ();
            ignore(Configure.check_ocaml ());
            let project = Analyze.prepare project_config [] in
            let bstate = Prepare.init project in
            Build.build_exe bstate exe;
            let files = Build.get_destination_files exe.Executable.target in
            List.iter (fun file ->
                printf "copying %s to %s\n" (fp_to_string (buildDir </> file)) (fp_to_string $ in_current_dir file);
                if(file_or_link_exists (fp_to_string $ in_current_dir file)) then
                  Unix.unlink (fp_to_string $ in_current_dir file); 
                Filesystem.copy_file (buildDir </> file) (in_current_dir file)
            ) files
        ) (fun () -> if !removeDist then Filesystem.removeDir tmpDir)
    with exn -> Exception.show exn

let () =
    try main ()
    with
        | NoMain -> eprintf "error: missing main argument, expecting one ml file as parameter\n"; exit 1
        | TooManyArgs -> eprintf "too many arguments, expecting just one ml file as parameter\n"; exit 1
