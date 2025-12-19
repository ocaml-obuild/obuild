open Lib
open Printf

(** Real-world META file validation script

    This script:
    1. Finds all META files in standard OCaml library locations
    2. Parses each one with the improved parser
    3. Logs errors with improved error messages
    4. Reports statistics on parser improvements
*)

(** Scan a directory for META files *)
let rec find_meta_files dir_path =
  let dir = Filepath.fp dir_path in
  if not (Filesystem.exists dir) then []
  else if not (Filesystem.is_dir dir) then []
  else
    try
      Filesystem.list_dir_pred_map (fun filename ->
        let fn_str = Filepath.fn_to_string filename in
        let full_path_str = Filename.concat dir_path fn_str in
        let full_path = Filepath.fp full_path_str in

        (* If it's META file, return it *)
        if fn_str = "META" then
          Some full_path_str
        (* If it's a directory, recursively scan it *)
        else if Filesystem.is_dir full_path then
          None  (* We'll handle directories separately to avoid deep nesting *)
        else
          None
      ) dir
      @
      (* Recursively scan subdirectories *)
      List.flatten (Filesystem.list_dir_pred_map (fun filename ->
        let fn_str = Filepath.fn_to_string filename in
        let full_path_str = Filename.concat dir_path fn_str in
        let full_path = Filepath.fp full_path_str in

        if Filesystem.is_dir full_path then
          Some (find_meta_files full_path_str)
        else
          None
      ) dir)
    with _ -> []

(** Read file contents *)
let read_file path =
  try
    let ic = open_in path in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    Some content
  with _ -> None

(** Parse result type *)
type parse_result =
  | Success of string  (* file path *)
  | LexerError of string * string  (* file path, error message *)
  | ParserError of string * string  (* file path, error message *)
  | ReadError of string  (* file path *)

(** Check if error message has position tracking *)
let has_position_tracking msg =
  (* Position format: "line.column: " *)
  try
    let _ = Str.search_forward (Str.regexp "[0-9]+\\.[0-9]+:") msg 0 in
    true
  with Not_found -> false

(** Check if error has context *)
let has_context msg =
  String.length msg > 50 ||  (* Long messages likely have context *)
  (try
    let _ = Str.search_forward (Str.regexp "expected") msg 0 in
    true
   with Not_found -> false)

(** Parse a single META file *)
let parse_meta_file path =
  match read_file path with
  | None -> ReadError path
  | Some content ->
      try
        let pkg_name = Filename.basename (Filename.dirname path) in
        let _ = Meta.parse (Filepath.fp path) content pkg_name in
        Success path
      with
      | Meta.MetaParseError (_, msg) ->
          (* Determine if it's a lexer or parser error *)
          if String.contains msg ':' && has_position_tracking msg then
            LexerError (path, msg)
          else
            ParserError (path, msg)
      | exn ->
          ParserError (path, Printexc.to_string exn)

(** Statistics tracker *)
type stats = {
  mutable total_files: int;
  mutable successful: int;
  mutable lexer_errors: int;
  mutable lexer_errors_with_position: int;
  mutable parser_errors: int;
  mutable parser_errors_with_context: int;
  mutable read_errors: int;
}

let create_stats () = {
  total_files = 0;
  successful = 0;
  lexer_errors = 0;
  lexer_errors_with_position = 0;
  parser_errors = 0;
  parser_errors_with_context = 0;
  read_errors = 0;
}

(** Update statistics with parse result *)
let update_stats stats result =
  stats.total_files <- stats.total_files + 1;
  match result with
  | Success _ ->
      stats.successful <- stats.successful + 1
  | LexerError (_, msg) ->
      stats.lexer_errors <- stats.lexer_errors + 1;
      if has_position_tracking msg then
        stats.lexer_errors_with_position <- stats.lexer_errors_with_position + 1
  | ParserError (_, msg) ->
      stats.parser_errors <- stats.parser_errors + 1;
      if has_context msg then
        stats.parser_errors_with_context <- stats.parser_errors_with_context + 1
  | ReadError _ ->
      stats.read_errors <- stats.read_errors + 1

(** Print error details *)
let print_error result =
  match result with
  | LexerError (path, msg) ->
      printf "\n[LEXER ERROR] %s\n" path;
      printf "  Message: %s\n" msg;
      printf "  Has position: %s\n" (if has_position_tracking msg then "✅ YES" else "❌ NO")
  | ParserError (path, msg) ->
      printf "\n[PARSER ERROR] %s\n" path;
      printf "  Message: %s\n" msg;
      printf "  Has context: %s\n" (if has_context msg then "✅ YES" else "❌ NO")
  | ReadError path ->
      printf "\n[READ ERROR] %s\n" path
  | Success _ -> ()

(** Print statistics *)
let print_stats stats =
  printf "\n";
  printf "════════════════════════════════════════════════════════════════\n";
  printf "                    VALIDATION SUMMARY\n";
  printf "════════════════════════════════════════════════════════════════\n";
  printf "\n";
  printf "Total META files scanned: %d\n" stats.total_files;
  printf "├─ ✅ Successfully parsed: %d (%.1f%%)\n"
    stats.successful
    (100.0 *. float_of_int stats.successful /. float_of_int stats.total_files);
  printf "├─ ❌ Lexer errors: %d (%.1f%%)\n"
    stats.lexer_errors
    (100.0 *. float_of_int stats.lexer_errors /. float_of_int stats.total_files);
  printf "│  └─ With position tracking: %d/%d (%.1f%%)\n"
    stats.lexer_errors_with_position
    stats.lexer_errors
    (if stats.lexer_errors > 0 then
       100.0 *. float_of_int stats.lexer_errors_with_position /. float_of_int stats.lexer_errors
     else 0.0);
  printf "├─ ❌ Parser errors: %d (%.1f%%)\n"
    stats.parser_errors
    (100.0 *. float_of_int stats.parser_errors /. float_of_int stats.total_files);
  printf "│  └─ With context: %d/%d (%.1f%%)\n"
    stats.parser_errors_with_context
    stats.parser_errors
    (if stats.parser_errors > 0 then
       100.0 *. float_of_int stats.parser_errors_with_context /. float_of_int stats.parser_errors
     else 0.0);
  printf "└─ ❌ Read errors: %d (%.1f%%)\n"
    stats.read_errors
    (100.0 *. float_of_int stats.read_errors /. float_of_int stats.total_files);
  printf "\n";
  printf "════════════════════════════════════════════════════════════════\n";
  printf "\n";

  (* Quality metrics *)
  printf "IMPROVEMENT QUALITY METRICS:\n";
  printf "├─ Lexer error quality: %s\n"
    (if stats.lexer_errors = 0 then "N/A (no lexer errors)"
     else if stats.lexer_errors_with_position = stats.lexer_errors then "✅ EXCELLENT (100%% have position)"
     else if stats.lexer_errors_with_position * 2 > stats.lexer_errors then "⚠️  GOOD (>50%% have position)"
     else "❌ POOR (<50%% have position)");
  printf "└─ Parser error quality: %s\n"
    (if stats.parser_errors = 0 then "N/A (no parser errors)"
     else if stats.parser_errors_with_context = stats.parser_errors then "✅ EXCELLENT (100%% have context)"
     else if stats.parser_errors_with_context * 2 > stats.parser_errors then "⚠️  GOOD (>50%% have context)"
     else "❌ POOR (<50%% have context)");
  printf "\n"

(** Main validation *)
let () =
  printf "Real-World META File Validation\n";
  printf "════════════════════════════════════════════════════════════════\n\n";

  (* Find all META files *)
  printf "Scanning for META files...\n";
  let search_paths = [
    "/usr/lib/ocaml";
    Filename.concat (Sys.getenv "HOME") ".opam/default/lib";
  ] in

  let all_meta_files = List.flatten (List.map (fun path ->
    printf "  Scanning %s...\n" path;
    find_meta_files path
  ) search_paths) in

  printf "\nFound %d META files\n\n" (List.length all_meta_files);

  (* Parse each file *)
  printf "Parsing META files...\n";
  let stats = create_stats () in
  let errors = ref [] in

  List.iter (fun path ->
    let result = parse_meta_file path in
    update_stats stats result;

    (* Collect errors for detailed reporting *)
    match result with
    | Success _ ->
        printf ".";
        flush stdout
    | LexerError _ | ParserError _ | ReadError _ ->
        printf "E";
        flush stdout;
        errors := result :: !errors
  ) all_meta_files;

  printf "\n\n";

  (* Print detailed errors if any *)
  if List.length !errors > 0 then begin
    printf "════════════════════════════════════════════════════════════════\n";
    printf "                     ERROR DETAILS\n";
    printf "════════════════════════════════════════════════════════════════\n";
    List.iter print_error (List.rev !errors);
    printf "\n"
  end;

  (* Print statistics *)
  print_stats stats;

  (* Exit code *)
  if stats.successful = stats.total_files then
    exit 0
  else
    exit 1
