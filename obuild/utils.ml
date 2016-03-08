open Ext.Fugue
open Ext.Filepath
open Ext
open Types

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

let parseCSV value = List.filter (fun s -> (String.length s) > 0) (List.map string_stripSpaces (string_split ',' value))

let to_include_path_options paths =
  let ss = ref StringSet.empty in
  List.concat $ list_filter_map (fun p -> let ps = fp_to_string p in
                                  if (ps = "") || (StringSet.mem ps !ss) || not (Filesystem.exists p) then None
                                  else (
                                    ss := StringSet.add ps !ss;
                                    Some ["-I"; ps]
                                  )) paths

let showList sep f l = String.concat sep (List.map f l)

let isWindows = Sys.os_type = "Win32"

let to_exe_name mode build name =
    let ext = extDP mode in
    let ext2 =
        match build with
        | ByteCode -> ".byte"
        | Native   -> if (Gconf.get_target_option "executable-as-obj") then ".o" else ""
        in
    fn (name ^ ext ^ ext2 ^ (if isWindows then ".exe" else ""))

exception FileNotFoundInPaths of (filepath list * filename)
exception FilesNotFoundInPaths of (filepath list * filepath list)

let get_system_paths () =
  let sep = if isWindows then ';' else ':' in
  try List.map fp (string_split sep (Sys.getenv "PATH"))
  with Not_found -> List.map fp ["/usr/bin"; "/usr/local/bin"]

let find_in_paths paths name =
    try List.find (fun p -> Filesystem.exists (p </> name)) paths
    with Not_found -> raise (FileNotFoundInPaths (paths, name))

let find_choice_in_paths paths names =
    try List.find (fun p ->
           try let _ = List.find (fun n -> Filesystem.exists (n p)) names in true
           with Not_found -> false
        ) paths
    with Not_found -> raise (FilesNotFoundInPaths (paths, (List.map (fun n -> n (List.hd paths)) names)))

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

