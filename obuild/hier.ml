open Ext.Fugue
open Ext.Filepath
open Types
open Modname

exception EmptyModuleHierarchy

type hier = { _hier : modname list }

let hiers = Hashtbl.create 128

let hier_root x = List.hd x._hier
let hier_parent x =
    match x._hier with
    | []  -> assert false
    | [_] -> None
    | l   -> Some { _hier = list_init l }

let hier_leaf x = list_last x._hier
let hier l = if l = [] then raise EmptyModuleHierarchy else { _hier = l }
let hier_lvl x = List.length x._hier - 1

let hier_to_string x = String.concat "." (List.map modname_to_string x._hier)
let hier_of_string x =
    let l = string_split '.' x in
    hier (List.map modname_of_string l)

let hier_of_modname x = { _hier = [x] }

let hier_to_node x = x._hier

let hier_to_dirpath x =
    if List.length x._hier > 1
        then fp (String.concat Filename.dir_sep (List.map modname_to_dir $ list_init x._hier))
        else currentDir

let hier_append x m = { _hier = x._hier @ [m] }

let add_prefix prefix_path hier =
  if List.length hier._hier <= 1 then 
    prefix_path
  else begin
    let to_fp =
      fp (String.concat Filename.dir_sep (List.map modname_to_dir $ list_init hier._hier)) in
    if (path_length prefix_path) = 0 then
      to_fp 
    else
      let rec loop path hier_list =
	match hier_list with
	  [] -> path <//> to_fp
	| x :: xs ->
	  if (path_basename path) = fn (modname_to_dir (List.hd hier_list)) then
	    if (path_length prefix_path) = 1 then
	      to_fp (* prefix_path is fully included in hier *)
	    else
	      loop (path_dirname path) (List.tl hier_list)
	  else
	    path <//> to_fp
      in
      loop prefix_path hier._hier
  end

let check_file path filename ext =
  if ext <>  Filetype.FileOther "" then
    Ext.Filesystem.exists (path </> ((fn filename) <.> (Filetype.file_type_to_string ext)))
  else
    Ext.Filesystem.exists (path </> (fn filename))

let get_filename path hier ext = 
  if Hashtbl.mem hiers hier then Hashtbl.find hiers hier
  else begin
    let modname = modname_to_string (hier_leaf hier) in
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

let add_hier modname filename =
  let h = hier [modname] in
  Hashtbl.add hiers h filename;
  h

let get_filepath path hier ext =
  let path = add_prefix path hier in
  path </> ((fn (get_filename path hier ext)) <.> (Filetype.file_type_to_string ext))

let get_dirpath path hier =
  let path = add_prefix path hier in
  path </> (fn (get_filename path hier (Filetype.FileOther "")))

let filename_of_hier hier prefix_path = get_filepath prefix_path hier Filetype.FileML
let directory_of_hier hier prefix_path = get_dirpath prefix_path hier
let generators_of_hier = List.map (fun gen hier prefix_path ->
    get_filepath prefix_path hier (Filetype.FileOther gen.Generators.suffix)
  ) !Generators.generators
let interface_of_hier hier prefix_path = get_filepath prefix_path hier Filetype.FileMLI

let cmx_of_hier prefix_path hier = get_filepath prefix_path hier Filetype.FileCMX
let cmo_of_hier prefix_path hier = get_filepath prefix_path hier Filetype.FileCMO
let cmc_of_hier bmode prefix_path hier = if bmode = Native then
    cmx_of_hier prefix_path hier
  else
    cmo_of_hier prefix_path hier

let cmi_of_hier prefix_path hier = get_filepath prefix_path hier Filetype.FileCMI

let module_lookup_methods = directory_of_hier::generators_of_hier @ [filename_of_hier]

let hier_of_directory filename =
  let name = fn_to_string filename in
  let m = wrap_module (String.capitalize name) in
  add_hier m name

let hier_of_filename filename =
  let name = Filename.chop_extension (fn_to_string filename) in
  let m = try wrap_module (String.capitalize name)
    with EmptyModuleName -> raise (ModuleFilenameNotValid (fn_to_string filename))
    | Invalid_argument _ -> raise (ModuleFilenameNotValid (fn_to_string filename))
  in
  add_hier m name;
