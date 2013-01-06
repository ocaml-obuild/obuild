exception EmptyFilename
exception InvalidFilename of string

type filepath = { filepath : string }
type filename = { filename : string }

let fp_to_string x = x.filepath
let fn_to_string x = x.filename

let filepath_to_string x = x.filepath
let filename_to_string x = x.filename

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
    let len = String.length x in
    let dirsepLen = String.length (Filename.dir_sep) in
    if x = "."
        then { filepath = "" }
        else (if len > dirsepLen && String.sub x (len-dirsepLen) dirsepLen = Filename.dir_sep
                then { filepath = String.sub x 0 (len-dirsepLen) }
                else { filepath = x }
        )

let fn x =
    if String.length x = 0 then (raise EmptyFilename)
    else if got_dirsep x then (raise (InvalidFilename x)) 
    else { filename = x }

let (<//>) (afp:filepath) (bfp:filepath) =
    let a = afp.filepath in
    let b = bfp.filepath in
    let len = String.length a in
    if len > 0 || a <> "."
        then fp (a ^ Filename.dir_sep ^ b)
        else bfp

let (</>) (afp:filepath) (bfp:filename) =
    afp <//> fp bfp.filename

let (<.>) (afp:filename) ext = fn (afp.filename ^ "." ^ ext)

let emptyDir = fp ""

let wrap_filepath x = fp x
let wrap_filename x = fn x

let with_optpath mdir (filename : filename) =
    let path =
        match mdir with
        | None     -> fp ""
        | Some dir -> dir
        in
    path </> filename

let with_path dir filename = dir </> filename

let path_dirname path = fp (Filename.dirname (fp_to_string path))
let path_basename path = fn (Filename.basename (fp_to_string path))

let in_current_dir (x:filename) = fp x.filename
