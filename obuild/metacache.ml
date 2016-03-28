open Meta
open Gconf
open Helper

let pkgs_cache : (string, Meta.t) Hashtbl.t = Hashtbl.create 100

let get_from_disk name =
  verbose Debug "  fetching META %s\n%!" name;
  try
    Meta.findLib name
  with Meta.LibraryNotFound n ->
    raise (Dependencies.DependencyMissing n)

let get name =
  try
    Hashtbl.find pkgs_cache name
  with Not_found ->
    let r = get_from_disk name in
    Hashtbl.add pkgs_cache name r;
    r

let get_from_cache lib =
  try
    Hashtbl.find pkgs_cache lib.Libname.main_name
  with Not_found ->
    failwith (Printf.sprintf "package %s not found in the hashtbl: internal error" (Libname.to_string lib))

let add name meta =
  Hashtbl.add pkgs_cache name meta

let find name =
  try
    Some (Hashtbl.find pkgs_cache name)
  with Not_found -> None
