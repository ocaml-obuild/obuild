open Filepath
open Filesystem

(** Build testing helper functions *)

(** Create a temporary directory for testing *)
let create_temp_dir prefix =
  let temp_base = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  let rec try_create n =
    if n > 100 then
      failwith "Could not create temporary directory after 100 attempts"
    else
      let dir_name = Printf.sprintf "%s/%s_%d_%d" temp_base prefix (Unix.getpid ()) n in
      try
        Unix.mkdir dir_name 0o755;
        dir_name
      with Unix.Unix_error (Unix.EEXIST, _, _) ->
        try_create (n + 1)
  in
  try_create 0

(** Remove directory recursively *)
let rec remove_dir_recursive dir =
  if Sys.file_exists dir then (
    if Sys.is_directory dir then (
      let entries = Sys.readdir dir in
      Array.iter (fun entry ->
        remove_dir_recursive (Filename.concat dir entry)
      ) entries;
      Unix.rmdir dir
    ) else
      Unix.unlink dir
  )

(** Write content to file, creating parent directories if needed *)
let write_file_with_dirs filepath content =
  let dir = Filename.dirname filepath in
  (* Create parent directories *)
  let rec create_parents path =
    if not (Sys.file_exists path) then (
      create_parents (Filename.dirname path);
      if not (Sys.file_exists path) then
        Unix.mkdir path 0o755
    )
  in
  create_parents dir;
  (* Write file *)
  let oc = open_out filepath in
  output_string oc content;
  close_out oc

(** Create temporary project with files and obuild config *)
let with_temp_build_project ~name ~files ~obuild_content ~test_fn =
  let temp_dir = create_temp_dir ("obuild_test_" ^ name) in
  try
    (* Write all project files *)
    List.iter (fun (filename, content) ->
      let filepath = Filename.concat temp_dir filename in
      write_file_with_dirs filepath content
    ) files;

    (* Write .obuild file *)
    let obuild_file = Filename.concat temp_dir (name ^ ".obuild") in
    write_file_with_dirs obuild_file obuild_content;

    (* Run the test function *)
    test_fn temp_dir;

    (* Cleanup *)
    remove_dir_recursive temp_dir
  with e ->
    (* Cleanup on error *)
    remove_dir_recursive temp_dir;
    raise e

(** Run obuild command and capture output *)
let run_obuild_command ~project_dir ~command ~args =
  let obuild_exe = Filename.concat (Unix.getcwd ()) "dist/build/obuild/obuild" in

  (* Build command line *)
  let cmd_args = obuild_exe :: command :: args in
  let cmd_line = String.concat " " (List.map Filename.quote cmd_args) in

  (* Save current directory *)
  let orig_dir = Unix.getcwd () in

  try
    (* Change to project directory *)
    Unix.chdir project_dir;

    (* Execute command and capture output *)
    let ic = Unix.open_process_in (cmd_line ^ " 2>&1") in
    let output = ref [] in
    (try
      while true do
        output := input_line ic :: !output
      done
    with End_of_file -> ());
    let status = Unix.close_process_in ic in

    (* Return to original directory *)
    Unix.chdir orig_dir;

    (* Check exit status *)
    let success = match status with
      | Unix.WEXITED 0 -> true
      | _ -> false
    in

    let output_str = String.concat "\n" (List.rev !output) in
    (success, output_str)

  with e ->
    (* Make sure we return to original directory *)
    Unix.chdir orig_dir;
    raise e

(** Get file modification time, returns None if file doesn't exist *)
let get_mtime filepath =
  try
    let stats = Unix.stat filepath in
    Some stats.Unix.st_mtime
  with Unix.Unix_error (Unix.ENOENT, _, _) ->
    None

(** Touch file to update its modification time *)
let touch_file filepath =
  let now = Unix.time () in
  Unix.utimes filepath now now

(** Sleep for a short duration (for mtime differences) *)
(** Use 2 seconds to ensure filesystem mtime resolution *)
let short_sleep () =
  ignore (Unix.select [] [] [] 1.1)

(** Assert that file exists *)
let assert_file_exists filepath =
  if not (Sys.file_exists filepath) then
    failwith (Printf.sprintf "Expected file to exist: %s" filepath)

(** Assert that file does not exist *)
let assert_file_not_exists filepath =
  if Sys.file_exists filepath then
    failwith (Printf.sprintf "Expected file to not exist: %s" filepath)

(** Assert that mtime1 < mtime2 *)
let assert_mtime_newer ~msg mtime1_opt mtime2_opt =
  match (mtime1_opt, mtime2_opt) with
  | (Some mtime1, Some mtime2) ->
      if not (mtime2 > mtime1) then
        failwith (Printf.sprintf "%s (mtime1=%.2f, mtime2=%.2f)" msg mtime1 mtime2)
  | (None, _) ->
      failwith (Printf.sprintf "%s (first file doesn't exist)" msg)
  | (_, None) ->
      failwith (Printf.sprintf "%s (second file doesn't exist)" msg)

(** Assert that mtime is unchanged *)
let assert_mtime_unchanged ~msg mtime1_opt mtime2_opt =
  match (mtime1_opt, mtime2_opt) with
  | (Some mtime1, Some mtime2) ->
      if mtime1 <> mtime2 then
        failwith (Printf.sprintf "%s (mtime changed from %.2f to %.2f)" msg mtime1 mtime2)
  | (None, _) ->
      failwith (Printf.sprintf "%s (first file doesn't exist)" msg)
  | (_, None) ->
      failwith (Printf.sprintf "%s (second file doesn't exist)" msg)
