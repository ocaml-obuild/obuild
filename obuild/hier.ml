open Filepath
open Types
open Ext.Fugue
open Modname

exception EmptyModuleHierarchy

type hier = { _hier : modname list }

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

let hier_to_node x = x._hier

let hier_to_dirpath x =
    if List.length x._hier > 1
        then fp (String.concat Filename.dir_sep (List.map modname_to_dir $ list_init x._hier))
        else currentDir

let hier_append x m = { _hier = x._hier @ [m] }

let filename_of_hier x  = hier_to_dirpath x </> filename_of_module (hier_leaf x)
let directory_of_hier x = hier_to_dirpath x </> directory_of_module (hier_leaf x)
let interface_of_hier x = hier_to_dirpath x </> interface_of_module (hier_leaf x)

let cmc_of_hier bmode x = hier_to_dirpath x </> cmc_of_module bmode (hier_leaf x)
let cmi_of_hier x = hier_to_dirpath x </> cmi_of_module (hier_leaf x)
