open Ext.Fugue
open Ext.Filepath
open Types

exception EmptyModuleHierarchy

type t = Modname.t list
(* first filepath is the source path, second is the actual path *)
type file_entry = FileEntry of (filepath * filepath) (* root_path, full_path *)
                | GeneratedFileEntry of (filepath * filepath * filename) (* root_path, full_path, generated_path *)
                | DirectoryEntry of (filepath * filepath) (* root_path, full_path *)

let file_entry_to_string = function
  | FileEntry (p, f) ->
      Printf.sprintf "FileEntry %s %s" (fp_to_string p) (fp_to_string f)
  | DirectoryEntry (p, f) ->
      Printf.sprintf "DirectoryEntry %s %s" (fp_to_string p) (fp_to_string f)
  | GeneratedFileEntry (p,f,n) ->
      Printf.sprintf "GeneratedFileEntry %s %s %s" (fp_to_string p) (fp_to_string f) (fn_to_string n)

let hiers : (t, file_entry) Hashtbl.t = Hashtbl.create 128

let root = List.hd
let parent x = match x with
  | []  -> assert false
  | [_] -> None
  | l   -> Some (list_init l)

let leaf = list_last
let make l = if l = [] then raise EmptyModuleHierarchy else l
let lvl x = List.length x - 1
let to_string x = String.concat "." (List.map Modname.to_string x)
let of_string x =
  let l = string_split '.' x in
  make (List.map Modname.of_string l)

let ml_to_ext path ext =
  let f = path_basename path in
  let d = path_dirname path in
  d </> ((chop_extension f) <.> (Filetype.to_string ext))

let of_modname x = [x]

let to_node x = x

let to_dirpath x =
  if List.length x > 1 then
    fp (String.concat Filename.dir_sep (List.map Modname.to_dir $ list_init x))
  else
    currentDir

let append x m = x @ [m]

let add_prefix prefix_path hier =
  if List.length hier <= 1 then 
    prefix_path
  else begin
    let to_fp =
      fp (String.concat Filename.dir_sep (List.map Modname.to_dir $ list_init hier)) in
    if (path_length prefix_path) = 0 then
      to_fp 
    else
      let rec loop path hier_list =
        match hier_list with
        | [] -> path <//> to_fp
        | x :: xs ->
          if (path_basename path) = fn (Modname.to_dir x) then
            if (path_length prefix_path) = 1 then
              to_fp (* prefix_path is fully included in hier *)
            else
              loop (path_dirname path) xs
          else
            path <//> to_fp
      in
      loop prefix_path (List.tl (List.rev hier))
  end

let check_file path filename ext =
  if ext <> Filetype.FileOther "" then
    Ext.Filesystem.exists (path </> ((fn filename) <.> (Filetype.to_string ext)))
  else
    Ext.Filesystem.exists (path </> (fn filename))

let check_modname path modname ext =
  if (check_file path modname ext) then
    Some modname
  else
    let name = String.uncapitalize modname in
    if (check_file path name ext) then
      Some name
    else
      None

let get_filepath root_path hier ext : file_entry option =
  if (Hashtbl.mem hiers hier) then
    Some (Hashtbl.find hiers hier)
  else
    let path = add_prefix root_path hier in
    let modname = Modname.to_string (leaf hier) in
    let res = check_modname path modname ext in
    match res with
    | None -> None
    | Some name ->
      let entry = if ext <> Filetype.FileOther "" then
          FileEntry (root_path, path </> ((fn name) <.> (Filetype.to_string ext)))
        else
          DirectoryEntry (root_path, path </> (fn name))
      in
      Hashtbl.add hiers hier entry;
      Some entry

let to_filename hier prefix_path = get_filepath prefix_path hier Filetype.FileML
let to_directory hier prefix_path = get_filepath prefix_path hier (Filetype.FileOther "")
let to_generators hier prefix_path =
  if (Hashtbl.mem hiers hier) then
    Some (Hashtbl.find hiers hier)
  else
    try
      Some (list_findmap (fun gen ->
          let path = add_prefix prefix_path hier in
          let modname = Modname.to_string (leaf hier) in
          let modname = gen.Generators.modname modname in
          let ext = Filetype.FileOther gen.Generators.suffix in
          let res = check_modname path modname ext in
          match res with
          | None -> None
          | Some name ->
            let filename = (fn name) <.> (Filetype.to_string ext) in
            let fullname = path </> filename in
            let generated_file = gen.Generators.generated_files filename (Modname.to_string (leaf hier)) in
            Hashtbl.add hiers hier (GeneratedFileEntry (prefix_path, fullname, generated_file));
            Some (GeneratedFileEntry (prefix_path, fullname, generated_file))
        ) !Generators.generators)
    with Not_found -> None

let get_src_file dst_dir = function
  | FileEntry (_,f) -> f
  | GeneratedFileEntry (_,_,fn) -> dst_dir </> fn
  | DirectoryEntry (_,f) -> f

let get_dest_file dst_dir ext hier =
  let entry = Hashtbl.find hiers hier in
  match entry with
    | FileEntry (_,f) ->
      let filename = path_basename f in
      let path = add_prefix dst_dir hier in
      path </> ((chop_extension filename) <.> Filetype.to_string ext)
    | GeneratedFileEntry (_,_,filename) ->
      let path = add_prefix dst_dir hier in
      path </> ((chop_extension filename) <.> Filetype.to_string ext)
    | DirectoryEntry (_,f) ->
      let filename = path_basename f in
      let path = add_prefix dst_dir hier in
      path </> (filename <.> Filetype.to_string ext)

let get_dest_file_ext dst_dir hier ext_f =
  let entry = Hashtbl.find hiers hier in
  match entry with
    | FileEntry (_,f) ->
      let filename = path_basename f in
      let filetype = Filetype.of_filepath f in
      let path = add_prefix dst_dir hier in
      path </> ((chop_extension filename) <.> Filetype.to_string (ext_f filetype))
    | GeneratedFileEntry (_,_,filename) ->
      let path = add_prefix dst_dir hier in
      let filetype = Filetype.of_filename filename in
      path </> ((chop_extension filename) <.> Filetype.to_string (ext_f filetype))
    | DirectoryEntry (_,f) ->
      let filename = path_basename f in
      let path = add_prefix dst_dir hier in
      let filetype = Filetype.of_filepath f in
      path </> (filename <.> Filetype.to_string (ext_f filetype))

let to_interface hier prefix_path = get_filepath prefix_path hier Filetype.FileMLI

let get_file_entry_maybe hier =
  if (Hashtbl.mem hiers hier) then
    Some (Hashtbl.find hiers hier)
  else
    None

let get_file_entry hier paths =
  if (Hashtbl.mem hiers hier) then
    Hashtbl.find hiers hier
  else
    list_findmap (fun path ->
        try
          Some (list_findmap (fun lookup -> lookup hier path) [to_filename; to_directory; to_generators; to_interface])
        with Not_found -> None
      ) paths

let of_filename filename =
  let name = Filename.chop_extension (fn_to_string filename) in
  let m = try Modname.wrap (String.capitalize name)
    with Modname.EmptyModuleName -> raise (Modname.ModuleFilenameNotValid (fn_to_string filename))
       | Invalid_argument _ -> raise (Modname.ModuleFilenameNotValid (fn_to_string filename))
  in
  make [m]
