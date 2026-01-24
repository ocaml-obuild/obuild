open Fugue
open Filepath
open Types

let read_file_with f filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      let z = f (input_line chan) in
      match z with
      | None -> ()
      | Some z' -> lines := z' :: !lines
    done;
    []
  with End_of_file ->
    close_in chan;
    List.rev !lines

let toKV line =
  match String_utils.split ~limit:2 ':' line with
  | [ k ] -> (String_utils.strip_spaces k, None)
  | [ k; v ] -> (String_utils.strip_spaces k, Some (String_utils.strip_spaces v))
  | _ -> assert false

let toKVeq line =
  match String_utils.split ~limit:2 '=' line with
  | [ k ] -> (String_utils.strip_spaces k, None)
  | [ k; v ] -> (String_utils.strip_spaces k, Some (String_utils.strip_spaces v))
  | _ -> assert false

let parseCSV value =
  List.filter
    (fun s -> String.length s > 0)
    (List.map String_utils.strip_spaces (String_utils.split ',' value))

let to_include_path_options paths =
  let ss = ref StringSet.empty in
  List.concat
  $ list_filter_map
      (fun p ->
        let ps = fp_to_string p in
        if ps = "" || StringSet.mem ps !ss || not (Filesystem.exists p) then
          None
        else (
          ss := StringSet.add ps !ss;
          Some [ "-I"; ps ]))
      paths

let showList sep f l = String.concat sep (List.map f l)
let isWindows = Sys.os_type = "Win32"

let to_exe_name mode build name =
  let ext = extDP mode in
  let ext2 =
    match build with
    | ByteCode -> ".byte"
    | Native -> if Gconf.get_target_option "executable-as-obj" then ".o" else ""
  in
  fn (name ^ ext ^ ext2 ^ if isWindows then ".exe" else "")

exception FileNotFoundInPaths of (filepath list * filename)
exception FilesNotFoundInPaths of (filepath list * filepath list)

let get_system_paths () =
  let sep = if isWindows then ';' else ':' in
  try List.map fp (String_utils.split sep (Sys.getenv "PATH"))
  with Not_found -> List.map fp [ "/usr/bin"; "/usr/local/bin" ]

let find_in_paths paths name =
  try List.find (fun p -> Filesystem.exists (p </> name)) paths
  with Not_found -> raise (FileNotFoundInPaths (paths, name))

let find_choice_in_paths paths names =
  try
    List.find
      (fun p ->
        try
          ignore (List.find (fun n -> Filesystem.exists (n p)) names);
          true
        with Not_found -> false)
      paths
  with Not_found ->
    raise (FilesNotFoundInPaths (paths, List.map (fun n -> n (List.hd paths)) names))

let exist_choice_in_paths paths names =
  try
    let _ = find_choice_in_paths paths names in
    true
  with FilesNotFoundInPaths _ -> false

let find_in_system_path name = find_in_paths (get_system_paths ()) name

let generateFile file f =
  let buffer = Buffer.create 1024 in
  f (Buffer.add_string buffer);
  Filesystem.write_file file (Buffer.contents buffer)

let get_cpu_count () =
  let read_command cmd =
    try
      let ic = Unix.open_process_in cmd in
      let line = input_line ic in
      let status = Unix.close_process_in ic in
      match status with
      | Unix.WEXITED 0 -> Some (int_of_string (String_utils.strip_spaces line))
      | _ -> None
    with _ -> None
  in

  let detected =
    match Sys.os_type with
    | "Unix" | "Cygwin" -> (
        (* Try different commands in order of preference *)
        match read_command "nproc 2>/dev/null" with
        (* Linux *)
        | Some n -> Some n
        | None -> (
            match read_command "sysctl -n hw.ncpu 2>/dev/null" with
            (* macOS, BSD *)
            | Some n -> Some n
            | None -> (
                match read_command "getconf _NPROCESSORS_ONLN 2>/dev/null" with
                (* POSIX *)
                | Some n -> Some n
                | None -> None)))
    | "Win32" -> (
        (* Windows: use NUMBER_OF_PROCESSORS environment variable *)
        try Some (int_of_string (Sys.getenv "NUMBER_OF_PROCESSORS")) with _ -> None)
    | _ -> None (* Unknown OS *)
  in

  match detected with
  | Some n when n > 0 && n <= 128 -> n (* Sanity check: reasonable CPU count *)
  | _ -> 2 (* Default fallback *)
