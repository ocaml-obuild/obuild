open Filepath

type t = {
  path : filepath list;
  destdir : filepath option;
  _all : (string * string option) list; (* kept for potential future use *)
  _loaded : bool; (* kept for potential future use *)
}

let default = { _all = []; path = []; destdir = None; _loaded = false }
let conf = ref default

let parse_file path =
  let content = Filesystem.read_file path in
  let unquote s =
    match s with
    | None -> failwith "unknown configuration key with no value"
    | Some x -> String_utils.init 1 (String_utils.drop 1 x)
  in
  let kvs = List.map Utils.toKVeq (String_utils.lines_noempty content) in
  let assoc_req key =
    try List.assoc key kvs
    with Not_found ->
      failwith ("findlib config " ^ fp_to_string path ^ ": missing key '" ^ key ^ "'")
  in
  (* Relocatable findlib (>= 1.9.9) writes RELATIVE path/destdir entries (".",
     "./ocaml"), which findlib resolves against the config FILE's own directory.
     obuild searches these dirs directly, so resolve them the same way — otherwise
     a relative entry is (wrongly) taken relative to obuild's CWD and no package is
     found. Absolute entries pass through unchanged (older findlib → no-op). *)
  let base = fp_to_string (path_dirname path) in
  let resolve s =
    let s = if String.length s >= 2 && s.[0] = '.' && s.[1] = '/' then String.sub s 2 (String.length s - 2) else s in
    if s = "" || s = "." then base
    else if Filename.is_relative s then Filename.concat base s
    else s
  in
  let paths = List.map (fun s -> fp (resolve s)) (String_utils.split ':' (unquote (assoc_req "path"))) in
  let destdir = resolve (unquote (assoc_req "destdir")) in
  { _all = kvs; path = paths; destdir = Some (fp destdir); _loaded = true }

let get_program_config () =
  match Process.run [ "ocamlfind"; "printconf"; "conf" ] with
  | Process.Failure (_, err, _) -> failwith ("ocamlfind printconf failed err " ^ err)
  | Process.Success (out, _, _) -> (
      match String_utils.lines_noempty out with
      | [ x ] -> [ fp x ]
      | _ -> failwith ("ocamlfind printconf failed output: " ^ out))

(* candidate locations of the findlib config file (distinct from the package search
   path returned by the exported get_paths below) *)
let get_conf_candidates () =
  try [ fp (Sys.getenv "OCAMLFIND_CONF") ]
  with Not_found -> (
    try get_program_config () with Failure _ | Not_found -> [ fp "/etc/findlib.conf"; fp "/etc/ocamlfind.conf" ])

let get_system () =
  (* an explicitly-set but missing OCAMLFIND_CONF is a mistake worth reporting: silently
     falling back to an empty config makes every dependency look "not found" *)
  (match (try Some (Sys.getenv "OCAMLFIND_CONF") with Not_found -> None) with
   | Some p when not (Filesystem.exists (fp p)) ->
       failwith ("OCAMLFIND_CONF is set to " ^ p ^ " but that file does not exist")
   | _ -> ());
  let paths = get_conf_candidates () in
  try
    let found_path = List.find Filesystem.exists paths in
    parse_file found_path
  with Not_found -> default

let load () =
  match Gconf.get_env "findlib-path" with
  | None -> conf := get_system ()
  | Some p -> conf := parse_file (fp p)

(* findlib prepends $OCAMLPATH to the search path; honor it so nix/esy/duniverse and
   OCAMLPATH-based opam layouts resolve the same packages obuild does. *)
let get_paths () =
  let sep = if Utils.isWindows then ';' else ':' in
  let env_paths =
    match (try Sys.getenv "OCAMLPATH" with Not_found -> "") with
    | "" -> []
    | s -> List.map fp (List.filter (fun x -> x <> "") (String_utils.split sep s))
  in
  env_paths @ !conf.path
let get_destdir () = !conf.destdir
