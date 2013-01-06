open Types
open Ext
open Filepath

exception NotADirectory
exception DoesntExist
exception SetupDoesntExist

let distPath = fp "dist"
let setupPath = distPath </> fn "setup"

let check f =
    if Filesystem.exists distPath
        then (if Sys.is_directory distPath.filepath
                then ()
                else raise NotADirectory
        ) else
            f ()

let checkOrFail () = check (fun () -> raise DoesntExist)
let checkOrCreate () = check (fun () -> let _ = Filesystem.mkdirSafe distPath 0o755 in ())

type buildType = Autogen | Dot | Library of string | Executable of string

let createBuildDest buildtype =
    let buildDir = distPath </> fn "build" in
    let _ = Filesystem.mkdirSafe buildDir 0o755 in
    let destDir =
        match buildtype with
        | Library l    -> buildDir </> fn ("lib-" ^ l)
        | Dot          -> buildDir </> fn ("dot")
        | Autogen      -> buildDir </> fn ("autogen")
        | Executable e -> buildDir </> fn e
        in
    let _ = Filesystem.mkdirSafe destDir 0o755 in
    destDir

let read_setup () =
    try
        let content = Filesystem.readFile setupPath in
        List.map (fun l -> second (default "") $ Utils.toKV l) $ string_split '\n' content
    with _ -> raise SetupDoesntExist

let write_setup setup =
    let kv (k,v) = k ^ ": " ^ v in
    Filesystem.writeFile setupPath (String.concat "\n" $ List.map kv setup)
