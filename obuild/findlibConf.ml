open Ext.Fugue
open Ext.Filepath
open Ext

type findlib_conf = {
  conf_path : filepath list;
  conf_destdir : filepath option;
  conf_all : (string * string option) list;
  conf_loaded : bool
}

let findlib_conf_default = {
  conf_all     = [];
  conf_path    = [];
  conf_destdir = None;
  conf_loaded  = false
}

let findlib_conf = ref findlib_conf_default

let parse_conf_file content =
  let unquote s = match s with
    | None   -> failwith ("unknown configuration key with no value")
    | Some x -> string_init 1 (string_drop 1 x)
  in
  let kvs = List.map Utils.toKVeq (string_lines_noempty content) in
  let paths = string_split ':' (unquote (List.assoc "path" kvs)) in
  let destdir = unquote (List.assoc "destdir" kvs) in
  {
      conf_all     = kvs;
      conf_path    = List.map fp paths;
      conf_destdir = Some (fp destdir);
      conf_loaded  = true;
    }

let get_conf path = parse_conf_file (Filesystem.readFile path)

let get_findlib_program_config () = match Process.run [ "ocamlfind"; "printconf"; "conf" ] with
  | Process.Failure err     -> failwith ("ocamlfind printconf failed err " ^ err)
  | Process.Success (out,_,_) -> match string_lines_noempty out with
    | [x] -> [fp x]
    | _   -> failwith ("ocamlfind printconf failed output: " ^ out)

let get_conf_paths () = try [fp (Sys.getenv "OCAMLFIND_CONF")]
  with Not_found ->
    try get_findlib_program_config ()
    with exn -> [
        fp "/etc/findlib.conf";
        fp "/etc/ocamlfind.conf"
      ]

let get_conf_system () = let allPaths = get_conf_paths () in
  try
    let found_path = List.find Filesystem.exists allPaths in
    get_conf found_path
  with Not_found -> findlib_conf_default

let load () = match Gconf.get_env ("findlib-path") with
  | None   -> findlib_conf := get_conf_system ()
  | Some p -> findlib_conf := get_conf (fp p)

let get_paths () = (!findlib_conf).conf_path

let get_destdir () = (!findlib_conf).conf_destdir
