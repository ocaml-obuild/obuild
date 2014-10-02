open Ext.Fugue
open Ext.Filepath
open Ext

type t = {
  path : filepath list;
  destdir : filepath option;
  all : (string * string option) list;
  loaded : bool
}

let default = {
  all     = [];
  path    = [];
  destdir = None;
  loaded  = false
}

let conf = ref default

let parse_file path =
  let content = Filesystem.readFile path in
  let unquote s = match s with
    | None   -> failwith ("unknown configuration key with no value")
    | Some x -> string_init 1 (string_drop 1 x)
  in
  let kvs = List.map Utils.toKVeq (string_lines_noempty content) in
  let paths = string_split ':' (unquote (List.assoc "path" kvs)) in
  let destdir = unquote (List.assoc "destdir" kvs) in
  {
    all     = kvs;
    path    = List.map fp paths;
    destdir = Some (fp destdir);
    loaded  = true;
  }

let get_program_config () = match Process.run [ "ocamlfind"; "printconf"; "conf" ] with
  | Process.Failure err     -> failwith ("ocamlfind printconf failed err " ^ err)
  | Process.Success (out,_,_) -> match string_lines_noempty out with
    | [x] -> [fp x]
    | _   -> failwith ("ocamlfind printconf failed output: " ^ out)
               
let get_paths () = try [fp (Sys.getenv "OCAMLFIND_CONF")]
  with Not_found ->
    try get_program_config ()
    with _ -> [
        fp "/etc/findlib.conf";
        fp "/etc/ocamlfind.conf"
      ]

let get_system () = let paths = get_paths () in
  try
    let found_path = List.find Filesystem.exists paths in
    parse_file found_path
  with Not_found -> default

let load () = match Gconf.get_env ("findlib-path") with
  | None   -> conf := get_system ()
  | Some p -> conf := parse_file (fp p)

let get_paths () = (!conf).path
let get_destdir () = (!conf).destdir
