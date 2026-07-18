open Gconf
open Helper

let initial_cache_size = 128

(* META cache, one per build invocation (carried in Analyze.project_config).
   No global state: caching is scoped to the build that populated it. *)
type t = (string, Meta.t) Hashtbl.t

let create () : t = Hashtbl.create initial_cache_size

let get_from_disk name =
  log Debug "  fetching META %s\n%!" name;
  try
    Meta.find_lib name
  with Meta.LibraryNotFound n ->
    raise (Dependencies.DependencyMissing n)

let get cache name =
  try
    Hashtbl.find cache name
  with Not_found ->
    let r = get_from_disk name in
    Hashtbl.add cache name r;
    r

let get_from_cache cache lib =
  try
    let (fp,pkg) = Hashtbl.find cache lib.Libname.main_name in
    (* Always return the root package - let callers do subpackage resolution *)
    (fp,pkg)
  with Not_found ->
    failwith (Printf.sprintf "package %s not found in the hashtbl: internal error" (Libname.to_string lib))

let add cache name meta =
  Hashtbl.add cache name meta

let find cache name =
  try
    Some (Hashtbl.find cache name)
  with Not_found -> None
