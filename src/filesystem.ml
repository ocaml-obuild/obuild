open Printf
open Ext
open Types
open Filepath

exception UnexpectedFileType
exception WriteFailed

let removeDirContent wpath =
    let path = wpath.filepath in
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
                        | _          -> raise UnexpectedFileType
            done;
        with End_of_file ->
            ()
        );
        Unix.closedir dirhandle;
        f path
        in
    rmdir_recursive (fun _ -> ()) path

let removeDir path = removeDirContent path; Unix.rmdir path.filepath; ()

let getModificationTime path =
   try (Unix.stat path.filepath).Unix.st_mtime
   with _ -> 0.0

let exists path = Sys.file_exists path.filepath

(* create a directory safely.
 *
 * return false if the directory already exists
 * return true if the directory has been created *)
let mkdirSafe path perm =
    if Sys.file_exists path.filepath
    then (if Sys.is_directory path.filepath
            then false
            else failwith ("directory " ^ path.filepath ^ " cannot be created: file already exists"))
    else (Unix.mkdir path.filepath perm; true)

let create_or_empty_dir path =
    let created = mkdirSafe path 0o755 in
    if not created then
        removeDirContent path;
    ()

let write_no_partial fd b o l =
    let len = ref l in
    let ofs = ref o in
    while !len > 0 do
        let written = Unix.write fd b !ofs !len in
        if written = 0 then raise WriteFailed;
        ofs := !ofs + written;
        len := !len - written
    done

let withfile path openflags perms f =
    let fd = Unix.openfile path.filepath openflags perms in
    finally (fun () -> f fd) (fun () -> Unix.close fd)

let writeFile path s =
    withfile path [Unix.O_WRONLY; Unix.O_CREAT] 0o644 (fun fd ->
        write_no_partial fd s 0 (String.length s)
    )

let readFile path =
    let buf = Buffer.create 1024 in
    let b = String.create 1024 in
    withfile path [Unix.O_RDONLY] 0o644 (fun fd ->
        let isDone = ref false in
        while not !isDone do
            let r = Unix.read fd b 0 1024 in
            if r > 0
                then Buffer.add_substring buf b 0 r
                else isDone := true
        done;
        Buffer.contents buf
    )

let copy_file src dst =
    let s = String.create 4096 in
    withfile dst [Unix.O_WRONLY; Unix.O_CREAT] 0o644 (fun fdDst ->
        withfile src [Unix.O_RDONLY] 0o644 (fun fdSrc ->
            let isDone = ref false in
            while not !isDone do
                let r = Unix.read fdSrc s 0 4096 in
                if r > 0
                    then write_no_partial fdDst s 0 r
                    else isDone := true
            done
        )
    )
