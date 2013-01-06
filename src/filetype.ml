open Filepath

type filetype = FileML
              | FileMLI
              | FileMLY
              | FileH
              | FileC
              | FileCMX
              | FileCMO
              | FileCMI
              | FileCMA
              | FileCMXA
              | FileO
              | FileA
              | FileSO
              | FileEXE
              | FileOther of string

let file_type_of_string s =
    match s with
    | "ml"   -> FileML
    | "mli"  -> FileMLI
    | "mly"  -> FileMLY
    | "h"    -> FileH
    | "c"    -> FileC
    | "cmx"  -> FileCMX
    | "cmo"  -> FileCMO
    | "cmi"  -> FileCMI
    | "cma"  -> FileCMA
    | "cmxa" -> FileCMXA
    | "o"    -> FileO
    | "a"    -> FileA
    | "so"   -> FileSO
    | "exe"  -> FileEXE
    | _      -> FileOther s

let file_type_to_string fty =
    match fty with
    | FileML      -> "ml"
    | FileMLI     -> "mli"
    | FileMLY     -> "mly"
    | FileH       -> "h"
    | FileC       -> "c"
    | FileCMX     -> "cmx"
    | FileCMO     -> "cmo"
    | FileCMI     -> "cmi"
    | FileCMA     -> "cma"
    | FileCMXA    -> "cmxa"
    | FileO       -> "o"
    | FileA       -> "a"
    | FileSO      -> "so"
    | FileEXE     -> "exe"
    | FileOther s -> s

type file_id = { fdep_ty   : filetype
               ; fdep_path : filepath
               }
let file_id (ty,p) = { fdep_ty = ty; fdep_path = p }

let get_extension (name : filename) : filetype =
    try
        let nameUnpack = fn_to_string name in
        let len = String.length (Filename.chop_extension nameUnpack) in
        (* +1 to remove the dot *)
        file_type_of_string (String.sub nameUnpack (len+1) (String.length nameUnpack - len - 1))
    with Invalid_argument _ -> FileEXE (* best effort, suit our case for unix *)

let get_extension_path (path : filepath) : filetype =
    get_extension (path_basename path)

let replace_extension (name:filename) ext =
    let extStr = file_type_to_string ext in
    try
        let choppedName = Filename.chop_extension (fn_to_string name) in
        fn (String.concat "." [ choppedName; extStr ])
    with Invalid_argument _ ->
        fn (fn_to_string name ^ "." ^ extStr)

let replace_extension_path path ext =
    let dir = path_dirname path in
    dir </> replace_extension (path_basename path) ext
