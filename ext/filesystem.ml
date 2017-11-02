open Printf
open Fugue
open Filepath
open Compat
exception UnexpectedFileType of string
exception WriteFailed

let removeDirContent wpath =
  let path = fp_to_string wpath in
  let rec rmdir_recursive f path =
    let dirhandle = Unix.opendir path in
    (try
       while true do
         let ent = Unix.readdir dirhandle in
         if String.length ent > 0 && ent.[0] <> '.'
         then
           let fent = path ^ Filename.dir_sep ^ ent in
           match (Unix.lstat fent).Unix.st_kind with
           | Unix.S_DIR -> rmdir_recursive (Unix.rmdir) fent
           | Unix.S_REG -> Unix.unlink fent
           | _          -> raise (UnexpectedFileType fent)
       done;
     with End_of_file ->
       ()
    );
    Unix.closedir dirhandle;
    f path
  in
  if Sys.file_exists path
  then
    rmdir_recursive (const ()) path

let removeDir path = removeDirContent path; Unix.rmdir (fp_to_string path); ()

let iterate f path =
    let dirhandle = Unix.opendir (fp_to_string path) in
    (try
        while true do
            let ent = Unix.readdir dirhandle in
            if ent <> ".." && ent <> "."
                then f (fn ent)
        done;
    with End_of_file ->
        ()
    );
    Unix.closedir dirhandle;
    ()

(* list directory entry with a map function included for efficiency *)
let list_dir_pred_map (p : filename -> 'a option) path : 'a list =
    let accum = ref [] in
    iterate (fun ent ->
        match p ent with
        | None   -> ()
        | Some e -> accum := e :: !accum
    ) path;
    !accum

let list_dir_pred (p : filename -> bool) path : filename list =
    list_dir_pred_map (fun e -> if p e then Some e else None) path

let list_dir = list_dir_pred (const true)

let list_dir_path_pred p path =
    let accum = ref [] in
    let dirhandle = Unix.opendir (fp_to_string path) in
    (try
        while true do
            let ent = Unix.readdir dirhandle in
            if ent <> ".." && p ent
                then accum := (path </> fn ent) :: !accum
        done;
    with End_of_file ->
        ()
    );
    Unix.closedir dirhandle;
    !accum

let list_dir_path = list_dir_path_pred (const true)

let getModificationTime path =
   try (Unix.stat (fp_to_string path)).Unix.st_mtime
   with _ -> 0.0

let exists path = Sys.file_exists (fp_to_string path)
let is_dir path =
    try Sys.is_directory (fp_to_string path)
    with _ -> false

(* create a directory safely.
 *
 * return false if the directory already exists
 * return true if the directory has been created *)
let mkdirSafe path perm =
    if Sys.file_exists (fp_to_string path)
    then (if Sys.is_directory (fp_to_string path)
            then false
            else failwith ("directory " ^ (fp_to_string path) ^ " cannot be created: file already exists"))
    else (Unix.mkdir (fp_to_string path) perm; true)

let mkdirSafe_ path perm =
    let (_: bool) = mkdirSafe path perm in
    ()

let rec mkdirSafeRecursive path perm =
    if not (is_dir path) then (
        if path_length path > 1 then (
            mkdirSafeRecursive (path_dirname path) perm;
            mkdirSafe_ path perm
        )
    )

let create_or_empty_dir path =
    let created = mkdirSafe path 0o755 in
    if not created then
        removeDirContent path;
    ()

let write_no_partial fd b o l =
    let len = ref l in
    let ofs = ref o in
    while !len > 0 do
        let written = Unix.write fd (bytes_of_string b) !ofs !len in
        if written = 0 then raise WriteFailed;
        ofs := !ofs + written;
        len := !len - written
    done

let withfile path openflags perms f =
    let fd = Unix.openfile (fp_to_string path) openflags perms in
    finally (fun () -> f fd) (fun () -> Unix.close fd)

let writeFile path s =
    withfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 (fun fd ->
        write_no_partial fd s 0 (String.length s)
    )

let readFile path =
    let buf = Buffer.create 1024 in
    let b = bytes_make 1024 ' ' in
    withfile path [Unix.O_RDONLY] 0o644 (fun fd ->
        let isDone = ref false in
        while not !isDone do
            let r = Unix.read fd b 0 1024 in
            if r > 0
                then buffer_add_subbytes buf b 0 r
                else isDone := true
        done;
        Buffer.contents buf
    )

let copy_file src dst =
    mkdirSafeRecursive (path_dirname dst) 0o755;
    let s = bytes_make 4096 ' ' in
    let srcStat = Unix.stat (fp_to_string src) in
    let operm = srcStat.Unix.st_perm in
    withfile dst [Unix.O_WRONLY; Unix.O_CREAT] operm (fun fdDst ->
        withfile src [Unix.O_RDONLY] 0o644 (fun fdSrc ->
            let isDone = ref false in
            while not !isDone do
                let r = Unix.read fdSrc s 0 4096 in
                if r > 0
                    then write_no_partial fdDst (bytes_to_string s) 0 r
                    else isDone := true
            done
        )
    )

let copy_to_dir src dst = copy_file src (dst <//> src)

let copy_many_files srcs dst = List.iter (fun src -> copy_to_dir src dst) srcs

let rec mktemp_dir_in prefix =
    let s = bytes_make 4 ' ' in
    let fd = Unix.openfile "/dev/urandom" [Unix.O_RDONLY] 0o640 in
    let r = ref 0 in
    while !r < 4 do
        let n = Unix.read fd s !r (4 - !r) in
        if n = 0
            then r := 4 (* should never happen, but even if it does, the getpid just provide basic randomness property *)
            else r := n + !r
    done;
    Unix.close fd;

    let s = bytes_to_string s in
    let tmpName = sprintf "%d-%02x%02x%02x%02x" (Unix.getpid ()) (Char.code s.[0]) (Char.code s.[1]) (Char.code s.[2]) (Char.code s.[3]) in
    let dirName = fp (prefix ^ tmpName) in
    let v = mkdirSafe dirName 0o755 in
    if v then dirName else mktemp_dir_in prefix
