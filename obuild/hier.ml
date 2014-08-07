open Ext.Fugue
open Ext.Filepath
open Types

exception EmptyModuleHierarchy

type t = Modname.t list

let hiers = Hashtbl.create 128

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
          if (path_basename path) = fn (Modname.to_dir (List.hd hier_list)) then
            if (path_length prefix_path) = 1 then
              to_fp (* prefix_path is fully included in hier *)
            else
              loop (path_dirname path) (List.tl hier_list)
          else
            path <//> to_fp
      in
      loop prefix_path (List.tl (List.rev hier))
  end

let check_file path filename ext =
  if ext <>  Filetype.FileOther "" then
    Ext.Filesystem.exists (path </> ((fn filename) <.> (Filetype.file_type_to_string ext)))
  else
    Ext.Filesystem.exists (path </> (fn filename))

let get_filename path hier ext = 
  if Hashtbl.mem hiers hier then Hashtbl.find hiers hier
  else begin
    let modname = Modname.to_string (leaf hier) in
    let filename = if (check_file path modname ext) then begin
        Hashtbl.add hiers hier modname;
        modname
      end else begin
        let name = String.uncapitalize modname in
        if (check_file path name ext) then
          Hashtbl.add hiers hier name;
        name
      end in
    filename
  end

let add modname filename =
  let h = make [modname] in
  Hashtbl.add hiers h filename;
  h

let get_filepath path hier ext =
  let path = add_prefix path hier in
  path </> ((fn (get_filename path hier ext)) <.> (Filetype.file_type_to_string ext))

let get_dirpath path hier =
  let path = add_prefix path hier in
  path </> (fn (get_filename path hier (Filetype.FileOther "")))

let to_filename hier prefix_path = get_filepath prefix_path hier Filetype.FileML
let to_directory hier prefix_path = get_dirpath prefix_path hier
let to_generators = List.map (fun gen hier prefix_path ->
    get_filepath prefix_path hier (Filetype.FileOther gen.Generators.suffix)
  ) !Generators.generators
let to_interface hier prefix_path = get_filepath prefix_path hier Filetype.FileMLI

let to_cmx prefix_path hier = get_filepath prefix_path hier Filetype.FileCMX
let to_cmo prefix_path hier = get_filepath prefix_path hier Filetype.FileCMO
let to_cmc bmode prefix_path hier = if bmode = Native then
    to_cmx prefix_path hier
  else
    to_cmo prefix_path hier

let to_cmi prefix_path hier = get_filepath prefix_path hier Filetype.FileCMI

let module_lookup_methods = to_directory::to_generators @ [to_filename]

let of_directory filename =
  let name = fn_to_string filename in
  let m = Modname.wrap (String.capitalize name) in
  add m name

let of_filename filename =
  let name = Filename.chop_extension (fn_to_string filename) in
  let m = try Modname.wrap (String.capitalize name)
    with Modname.EmptyModuleName -> raise (Modname.ModuleFilenameNotValid (fn_to_string filename))
    | Invalid_argument _ -> raise (Modname.ModuleFilenameNotValid (fn_to_string filename))
  in
  add m name;
