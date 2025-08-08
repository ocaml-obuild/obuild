open Ext.Fugue
open Ext.Filepath
open Ext

type t = Autogen | Dot | Target of Target.Name.t

let to_string = function
  | Autogen  -> "autogen"
  | Dot      -> "dot"
  | Target n -> "target(" ^ Target.Name.to_string n ^ ")"

let to_filename = function
  | Target tn    -> Target.Name.to_dirname tn
  | Dot          -> fn ("dot")
  | Autogen      -> fn ("autogen")

exception NotADirectory
exception MissingDestinationDirectory of t
exception DoesntExist
exception FileDoesntExist of string

let path = ref (fp "dist")

let set_path p = path := p
let get_path () = !path

let setup_path = get_path () </> fn "setup"
let configure_path = get_path () </> fn "configure"
let build_path = get_path () </> fn "build"

let check_exn f =
  if Filesystem.exists (get_path ()) then
    (if Sys.is_directory $ fp_to_string (get_path ()) then ()
     else raise NotADirectory)
  else
    f ()

let exist () = check_exn (fun () -> raise DoesntExist)
let create_maybe () = check_exn (fun () -> let _ = Filesystem.mkdirSafe (get_path ()) 0o755 in ())

let get_build () = get_path () </> fn "build"

let get_build_path buildtype =
    get_build () </> (to_filename buildtype)

let get_build_exn buildtype =
  let dist = get_build_path buildtype in
  if not (Filesystem.is_dir dist) then
    raise (MissingDestinationDirectory buildtype)
  else
    dist

let create_build buildtype =
    let _ = Filesystem.mkdirSafe (get_build ()) 0o755 in
    let dest = get_build_path buildtype in
    let _ = Filesystem.mkdirSafe dest 0o755 in
    dest

let read_dist_file path =
  try
    let content = Filesystem.readFile path in
    hashtbl_fromList (List.map (fun l -> second (default "") $ Utils.toKV l) $ string_split '\n' content)
  with _ -> raise (FileDoesntExist (fp_to_string path))

let read_setup () = read_dist_file setup_path
let read_configure () = read_dist_file configure_path

let write_setup setup =
    let kv (k,v) = k ^ ": " ^ v in
    Filesystem.writeFile setup_path (String.concat "\n" $ List.map kv (hashtbl_toList setup))

let remove_dead_links () =
  let files = Sys.readdir "." in
  let build_path = fp_to_string (get_build ()) in
  Array.iter (fun fn -> try
                 let l = Unix.readlink fn in
                 if (string_startswith build_path l) then
                     Sys.remove fn
               with _ -> ()) files
