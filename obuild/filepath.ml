open Ext.Fugue

exception EmptyFilename
exception InvalidFilename of string

type filepath = { absolute: bool; filepath : string list }
type filename = { filename : string }

let fp_to_string x =
    match x.filepath, x.absolute with
    | ([], true)  -> "/"
    | ([], false) -> "./"
    | (l,  true)  -> "/" ^ String.concat Filename.dir_sep x.filepath
    | (l,  false) -> String.concat Filename.dir_sep x.filepath

let fn_to_string x = x.filename

let got_dirsep x =
    let gotDirsep = ref false in
    let dirsepLen = String.length (Filename.dir_sep) in
    for i = 0 to String.length x - dirsepLen - 1
    do
        if String.sub x i dirsepLen = Filename.dir_sep
            then gotDirsep := true
    done;
    !gotDirsep

(* this only strip the last / if it exists *)
let fp x =
    (* TODO fix it properly, however separator is always a single char *)
    match string_split Filename.dir_sep.[0] x with
    | "" :: p ->
        { absolute = true; filepath = List.filter (fun x -> x <> "." && x <> "") p }
    | p ->
        { absolute = false; filepath = List.filter (fun x -> x <> "." && x <> "") p }

let fn x =
    if String.length x = 0 || x = "." then (raise EmptyFilename)
    else if got_dirsep x then (raise (InvalidFilename x)) 
    else { filename = x }

let valid_fn x = try let _ = fn x in true with _ -> false 

let (<//>) (afp:filepath) (bfp:filepath) =
    match (afp.absolute, bfp.absolute) with
    | (true, true)  -> failwith "cannot concat two absolute paths"
    | (false, true) -> failwith "cannot concat an absolute path to a relative path"
    | (_, _)        -> { absolute = afp.absolute; filepath = afp.filepath @ bfp.filepath }

let (</>) (afp:filepath) (bfp:filename) =
    { absolute = afp.absolute; filepath = afp.filepath @ [bfp.filename] }

let (<.>) (afp:filename) ext = fn (afp.filename ^ "." ^ ext)

let emptyDir = { absolute = false; filepath = [] }
let currentDir = emptyDir

let with_optpath mdir (filename : filename) =
    let path =
        match mdir with
        | None     -> currentDir
        | Some dir -> dir
        in
    path </> filename

let with_path dir filename = dir </> filename

let path_length path   = List.length path.filepath
let path_dirname path  = { path with filepath = list_init path.filepath }
let path_basename path = fn (list_last path.filepath)
let path_append path x = fp (fp_to_string path ^ x)

let path_parent path = path_dirname (path_dirname path)

let in_current_dir (x:filename) = fp x.filename

let chop_extension (x:filename) = fn (Filename.chop_extension (fn_to_string x))
