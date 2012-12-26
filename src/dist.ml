open Types
open Ext

exception NotADirectory
exception DoesntExist
exception SetupDoesntExist

let distPath = "dist"
let setupPath = distPath </> "setup"

type setup = (string * string) list

let check f =
    if Sys.file_exists distPath
        then (if Sys.is_directory distPath
                then ()
                else raise NotADirectory
        ) else
            f ()

let checkOrFail () = check (fun () -> raise DoesntExist)
let checkOrCreate () = check (fun () -> Unix.mkdir distPath 0o755)

let withBuildDirNew m f =
    let bdir = distPath </> "build" in
    Unix.mkdir bdir 0o755;
    (*
    Filesystem.removeDir bdir;
    Unix.mkdir
    *)
    f bdir

type buildType = Autogen | Library | Executable of string

let createBuildDest buildtype =
    let buildDir = distPath </> "build" in
    let _ = Filesystem.mkdirSafe buildDir 0o755 in
    match buildtype with
    | Library      -> buildDir
    | Autogen      ->
           let autoDir = buildDir </> "autogen" in
           let _ = Filesystem.mkdirSafe autoDir 0o755 in
           autoDir
    | Executable e ->
           let exeDir = buildDir </> e in
           let _ = Filesystem.mkdirSafe exeDir 0o755 in
           exeDir

let read_setup () =
    try
        let content = Filesystem.readFile setupPath in
        List.map (fun l -> second (default "") $ Utils.toKV l) $ string_split '\n' content
    with _ -> raise SetupDoesntExist

let write_setup setup =
    let kv (k,v) = k ^ ": " ^ v in
    Filesystem.writeFile setupPath (String.concat "\n" $ List.map kv setup)
