open Ext
open Filepath

type findlib_conf =
    { conf_path : filepath list
    ; conf_destdir : filepath option
    ; conf_all : (string * string option) list
    ; conf_loaded : bool
    }

let findlib_conf = ref
    { conf_all     = []
    ; conf_path    = []
    ; conf_destdir = None
    ; conf_loaded  = false
    }

let parseConfFile content =
    let unquote s =
        match s with
        | None   -> failwith ("unknown configuration key with no value")
        | Some x -> string_init 1 (string_drop 1 x)
        in
    let kvs = List.map Utils.toKVeq (string_lines_noempty content) in
    let paths = string_split ':' (unquote (List.assoc "path" kvs)) in
    let destdir = unquote (List.assoc "destdir" kvs) in
    { conf_all     = kvs
    ; conf_path    = List.map fp paths
    ; conf_destdir = Some (fp destdir)
    ; conf_loaded  = true
    }

let getConf path = parseConfFile (Filesystem.readFile path)

let getFindlibProgram_Config () =
    match Process.run_with_outputs [ "ocamlfind"; "printconf"; "conf" ] with
    | Process.Failure err     -> failwith ("ocamlfind printconf failed err " ^ err)
    | Process.Success (out,_) -> match string_lines_noempty out with
                         | [x] -> [fp x]
                         | _   -> failwith ("ocamlfind printconf failed output: " ^ out)

let getConfPaths () =
    try [fp (Sys.getenv "OCAMLFIND_CONF")]
    with Not_found ->
        try getFindlibProgram_Config ()
        with exn ->
            [ fp "/etc/findlib.conf"
            ; fp "/etc/ocamlfind.conf"
            ]

let getConfSystem () =
    let allPaths = getConfPaths () in
    try
        let foundPath = List.find Filesystem.exists allPaths in
        getConf foundPath
    with Not_found ->
        { conf_all     = []
        ; conf_path    = []
        ; conf_destdir = None
        ; conf_loaded  = false
        }

let load () =
    match Gconf.gconf.Gconf.conf_findlib_path with
    | None   -> findlib_conf := getConfSystem ()
    | Some p -> findlib_conf := getConf (fp p)

let get_paths () = (!findlib_conf).conf_path
