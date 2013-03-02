open Ext.Fugue
open Ext.Filepath
open Ext
open Types
open Gconf

type buildType = Autogen | Dot | Target of name

let buildtype_to_string ty =
    match ty with
    | Autogen  -> "autogen"
    | Dot      -> "dot"
    | Target n -> "target(" ^ name_to_string n ^ ")"

exception NotADirectory
exception MissingDestinationDirectory of buildType
exception DoesntExist
exception SetupDoesntExist

let distPath = ref (fp "dist")

let setDistPath p = distPath := p
let getDistPath () = !distPath

let setupPath = getDistPath () </> fn "setup"

let check f =
    if Filesystem.exists (getDistPath ())
        then (if Sys.is_directory $ fp_to_string (getDistPath ())
                then ()
                else raise NotADirectory
        ) else
            f ()

let checkOrFail () = check (fun () -> raise DoesntExist)
let checkOrCreate () = check (fun () -> let _ = Filesystem.mkdirSafe (getDistPath ()) 0o755 in ())

let get_target_dirname tname =
    match tname with
    | ExeName e | BenchName e | TestName e | ExampleName e -> fn e
    | LibName l -> fn ("lib-" ^ lib_name_to_string l)

let getBuildDest_path buildtype =
    let buildDir = getDistPath () </> fn "build" in
    match buildtype with
    | Target tn    -> buildDir </> get_target_dirname tn
    | Dot          -> buildDir </> fn ("dot")
    | Autogen      -> buildDir </> fn ("autogen")

let getBuildDest buildtype =
    let distPath = getBuildDest_path buildtype in
    if not (Filesystem.is_dir distPath)
        then raise (MissingDestinationDirectory buildtype)
        else distPath

let createBuildDest buildtype =
    let buildDir = getDistPath () </> fn "build" in
    let _ = Filesystem.mkdirSafe buildDir 0o755 in
    let destDir =
        match buildtype with
        | Target tn    -> buildDir </> get_target_dirname tn
        | Dot          -> buildDir </> fn ("dot")
        | Autogen      -> buildDir </> fn ("autogen")
        in
    let _ = Filesystem.mkdirSafe destDir 0o755 in
    destDir

let read_setup () =
    try
        let content = Filesystem.readFile setupPath in
        hashtbl_fromList (List.map (fun l -> second (default "") $ Utils.toKV l) $ string_split '\n' content)
    with _ ->
        raise SetupDoesntExist

let write_setup setup =
    let kv (k,v) = k ^ ": " ^ v in
    Filesystem.writeFile setupPath (String.concat "\n" $ List.map kv (hashtbl_toList setup))
