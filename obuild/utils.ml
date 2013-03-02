open Ext.Fugue
open Ext.Filepath
open Ext
open Types
open Gconf

let read_file_with f filename = 
    let lines = ref [] in
    let chan = open_in filename in
    try
        while true; do
            let z = f (input_line chan) in
            match z with
            | None    -> ()
            | Some z' -> lines := z' :: !lines
        done; []
    with End_of_file ->
        close_in chan;
        List.rev !lines

let toKV line =
    match string_split ~limit:2 ':' line with
    | [k]   -> (string_stripSpaces k, None)
    | [k;v] -> (string_stripSpaces k, Some (string_stripSpaces v))
    | _     -> assert false

let toKVeq line =
    match string_split ~limit:2 '=' line with
    | [k]   -> (string_stripSpaces k, None)
    | [k;v] -> (string_stripSpaces k, Some (string_stripSpaces v))
    | _     -> assert false

let parseCSV value = List.map string_stripSpaces (string_split ',' value)

let to_include_path_options paths =
    List.concat $ list_filter_map (fun p -> if fp_to_string p = "" then None else Some [ "-I"; fp_to_string p ]) paths

let showList sep f l = String.concat sep (List.map f l)

(* hacky way to detect windows *)
let is_windows = Filename.dir_sep <> "/"

let to_exe_name mode build name =
    let ext = extDP mode in
    let ext2 =
        match build with
        | ByteCode -> ".byte"
        | Native   -> if gconf.conf_executable_as_obj then ".o" else ""
        in
    fn (name ^ ext ^ ext2 ^ (if is_windows then ".exe" else ""))

exception FileNotFoundInPaths of (filepath list * filename)
exception FilesNotFoundInPaths of (filepath list * filename list)

let get_system_paths () =
    try List.map fp (string_split ':' (Sys.getenv "PATH"))
    with Not_found -> List.map fp ["/usr/bin"; "/usr/local/bin"]

let find_in_paths paths name =
    try List.find (fun p -> Filesystem.exists (p </> name)) paths
    with Not_found -> raise (FileNotFoundInPaths (paths, name))

let find_choice_in_paths paths names =
    try List.find (fun p ->
           try let _ = List.find (fun n -> Filesystem.exists (p </> n)) names in true
           with Not_found -> false
        ) paths
    with Not_found -> raise (FilesNotFoundInPaths (paths, names))

let exist_choice_in_paths paths names =
    try let _ = find_choice_in_paths paths names in true
    with FilesNotFoundInPaths _ -> false

let find_in_system_path name =
    find_in_paths (get_system_paths ()) name

let wrap_exn print fname f =
    try f ()
    with exn ->
        print "%s: %s\n%!" fname (Printexc.to_string exn);
        raise exn

let generateFile file f =
    let buffer = Buffer.create 1024 in
    f (Buffer.add_string buffer);
    Filesystem.writeFile file (Buffer.contents buffer)

