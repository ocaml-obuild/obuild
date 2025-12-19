open Printf
open Fugue
open Filepath
open Compat

exception UnexpectedFileType of string
exception WriteFailed

let remove_dir_content wpath =
  let path = fp_to_string wpath in
  let rec rmdir_recursive f path =
    let dirhandle = Unix.opendir path in
    (try
       while true do
         let ent = Unix.readdir dirhandle in
         if String.length ent > 0 && ent.[0] <> '.' then
           let fent = path ^ Filename.dir_sep ^ ent in
           match (Unix.lstat fent).Unix.st_kind with
           | Unix.S_DIR -> rmdir_recursive Unix.rmdir fent
           | Unix.S_REG -> Unix.unlink fent
           | _ -> raise (UnexpectedFileType fent)
       done
     with End_of_file -> ());
    Unix.closedir dirhandle;
    f path
  in
  if Sys.file_exists path then
    rmdir_recursive (const ()) path

let remove_dir path =
  remove_dir_content path;
  Unix.rmdir (fp_to_string path);
  ()

let iterate f path =
  let entries = Sys.readdir (fp_to_string path) in
  Array.fast_sort String.compare entries;
  Array.iter (fun ent -> f (fn ent)) entries;
  ()

(* list directory entry with a map function included for efficiency *)
let list_dir_pred_map (p : filename -> 'a option) path : 'a list =
  let accum = ref [] in
  iterate
    (fun ent ->
      match p ent with
      | None -> ()
      | Some e -> accum := e :: !accum)
    path;
  !accum

let list_dir_pred (p : filename -> bool) path : filename list =
  list_dir_pred_map (fun e -> if p e then Some e else None) path

let get_modification_time path = try (Unix.stat (fp_to_string path)).Unix.st_mtime with _ -> 0.0
let exists path = Sys.file_exists (fp_to_string path)
let is_dir path = try Sys.is_directory (fp_to_string path) with _ -> false

(* create a directory safely.
 *
 * return false if the directory already exists
 * return true if the directory has been created *)
let mkdir_safe path perm =
  if Sys.file_exists (fp_to_string path) then
    if Sys.is_directory (fp_to_string path) then
      false
    else
      failwith ("directory " ^ fp_to_string path ^ " cannot be created: file already exists")
  else (
    Unix.mkdir (fp_to_string path) perm;
    true)

let mkdir_safe_ path perm =
  let (_ : bool) = mkdir_safe path perm in
  ()

let rec mkdir_safe_recursive path perm =
  if not (is_dir path) then
    if path_length path > 1 then (
      mkdir_safe_recursive (path_dirname path) perm;
      mkdir_safe_ path perm)

(** [write_no_partial fd buf start len] writes [len] chars of [buf] starting at [start] in [fd], or
    raises [WriteFailed] if impossible. *)
let write_no_partial fd b o l =
  let len = ref l in
  let ofs = ref o in
  while !len > 0 do
    let written = Unix.write fd (bytes_of_string b) !ofs !len in
    if written = 0 then raise WriteFailed;
    ofs := !ofs + written;
    len := !len - written
  done

(** [with_file fp flags perms f] opens the file at [fp] and apply [f] to the obtained file
    descriptor. *)
let with_file path openflags perms f =
  let fd = Unix.openfile (fp_to_string path) openflags perms in
  finally (fun () -> f fd) (fun () -> Unix.close fd)

let write_file path s =
  with_file path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644 (fun fd ->
      write_no_partial fd s 0 (String.length s))

let read_file path =
  let buf = Buffer.create 1024 in
  let b = bytes_make 1024 ' ' in
  with_file path [ Unix.O_RDONLY ] 0o644 (fun fd ->
      let isDone = ref false in
      while not !isDone do
        let r = Unix.read fd b 0 1024 in
        if r > 0 then
          buffer_add_subbytes buf b 0 r
        else
          isDone := true
      done;
      Buffer.contents buf)

let copy_file src dst =
  mkdir_safe_recursive (path_dirname dst) 0o755;
  let s = bytes_make 4096 ' ' in
  let srcStat = Unix.stat (fp_to_string src) in
  let operm = srcStat.Unix.st_perm in
  with_file dst [ Unix.O_WRONLY; Unix.O_CREAT ] operm (fun fdDst ->
      with_file src [ Unix.O_RDONLY ] 0o644 (fun fdSrc ->
          let isDone = ref false in
          while not !isDone do
            let r = Unix.read fdSrc s 0 4096 in
            if r > 0 then
              write_no_partial fdDst (bytes_to_string s) 0 r
            else
              isDone := true
          done))

let copy_to_dir src dst = copy_file src (dst <//> src)
