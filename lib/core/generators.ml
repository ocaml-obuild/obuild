open Filepath
open Helper
open Gconf

exception GeneratorFailed of string
exception GeneratorNotFound of string

(** Generator match type - how to identify files for this generator *)
type match_type =
  | Match_suffix of string      (** Match by file extension (e.g., "mly") *)
  | Match_filename of string    (** Match by exact filename (e.g., "VERSION") *)
  | Match_pattern of string     (** Match by glob pattern (e.g., "*.txt") *)
  | Match_directory             (** Match directories *)

(** Built-in generator type (for backward compatibility) *)
type t = {
  suffix : string;
  modname : (Modname.t -> Modname.t);
  commands : (filepath -> filepath -> string -> string list list);
  generated_files : (filename -> string -> filename);
}

(** Custom generator definition from .obuild file *)
type custom = {
  custom_name : string;                   (** Generator name for reference *)
  custom_match : match_type;              (** How to match source files *)
  custom_command : string;                (** Command template with variables *)
  custom_outputs : string list;           (** Output file patterns *)
  custom_module_name : string option;     (** Module name pattern if different from base *)
  custom_multi_input : bool;              (** Whether this generator can take multiple inputs *)
}

(** Built-in generators - now empty, all generators defined in .obuild files *)
let builtin_generators : t list ref = ref []

(** Custom generators registered from project file *)
let custom_generators : custom list ref = ref []

(** Find substring in string, returns index or raises Not_found *)
let find_substring str sub =
  let len = String.length str in
  let sublen = String.length sub in
  if sublen = 0 then 0
  else if sublen > len then raise Not_found
  else
    let rec search i =
      if i + sublen > len then raise Not_found
      else if String.sub str i sublen = sub then i
      else search (i + 1)
    in
    search 0

(** Substitute variables in a string
    Variables supported:
    - ${src}     : Full path to source file
    - ${dest}    : Destination path without extension
    - ${base}    : Base filename without extension
    - ${srcdir}  : Source directory
    - ${destdir} : Destination directory
    - ${sources} : Space-separated list of all input files (for multi-input)
*)
let substitute_variables ~src ~dest ~sources str =
  let src_str = fp_to_string src in
  let dest_str = fp_to_string dest in
  let base = fn_to_string (chop_extension (path_basename src)) in
  let srcdir = fp_to_string (path_dirname src) in
  let destdir = fp_to_string (path_dirname dest) in
  let sources_str = String.concat " " (List.map fp_to_string sources) in

  let replacements = [
    ("${src}", src_str);
    ("${dest}", dest_str);
    ("${base}", base);
    ("${srcdir}", srcdir);
    ("${destdir}", destdir);
    ("${sources}", sources_str);
  ] in

  List.fold_left (fun s (var, value) ->
    let rec replace_all str =
      try
        let i = find_substring str var in
        let before = String.sub str 0 i in
        let after = String.sub str (i + String.length var) (String.length str - i - String.length var) in
        replace_all (before ^ value ^ after)
      with Not_found -> str
    in
    replace_all s
  ) str replacements

(** Substitute variables in output pattern *)
let substitute_output_pattern ~src pattern =
  let base = fn_to_string (chop_extension (path_basename src)) in
  let replacements = [
    ("${base}", base);
  ] in
  List.fold_left (fun s (var, value) ->
    let rec replace_all str =
      try
        let i = find_substring str var in
        let before = String.sub str 0 i in
        let after = String.sub str (i + String.length var) (String.length str - i - String.length var) in
        replace_all (before ^ value ^ after)
      with Not_found -> str
    in
    replace_all s
  ) pattern replacements

(** Parse command string into words, respecting quotes *)
let parse_command cmd =
  let len = String.length cmd in
  let rec loop acc current i in_quote =
    if i >= len then
      let final = Buffer.contents current in
      if final = "" then List.rev acc else List.rev (final :: acc)
    else
      let c = cmd.[i] in
      match c, in_quote with
      | '"', false -> loop acc current (i + 1) true
      | '"', true -> loop acc current (i + 1) false
      | ' ', false ->
          let word = Buffer.contents current in
          if word = "" then
            loop acc current (i + 1) false
          else begin
            Buffer.clear current;
            loop (word :: acc) current (i + 1) false
          end
      | _, _ ->
          Buffer.add_char current c;
          loop acc current (i + 1) in_quote
  in
  loop [] (Buffer.create 64) 0 false

(** Convert custom generator to built-in type for compatibility *)
let custom_to_builtin (custom : custom) : t =
  let suffix = match custom.custom_match with
    | Match_suffix s -> s
    | Match_filename _ -> ""  (* filename match handled separately *)
    | Match_pattern _ -> ""   (* pattern match handled separately *)
    | Match_directory -> ""   (* directory match handled separately *)
  in
  let modname = match custom.custom_module_name with
    | None -> (fun m -> m)
    | Some pattern ->
        (fun m ->
          let base = Compat.string_lowercase (Modname.to_string m) in
          let name = substitute_output_pattern ~src:(fp base) pattern in
          Modname.of_string (Compat.string_capitalize name))
  in
  let commands = fun src dest _moduleName ->
    let cmd = substitute_variables ~src ~dest ~sources:[src] custom.custom_command in
    (* Run command through shell to support shell features like &&, |, etc. *)
    [["sh"; "-c"; cmd]]
  in
  let generated_files = fun f _moduleName ->
    match custom.custom_outputs with
    | [] -> chop_extension f <.> "ml"  (* default to .ml *)
    | output :: _ ->
        let pattern = substitute_output_pattern ~src:(fp (fn_to_string f)) output in
        fn pattern
  in
  { suffix; modname; commands; generated_files }

(** Register a custom generator from project file *)
let register_custom (gen : custom) =
  custom_generators := gen :: !custom_generators

(** Register multiple custom generators *)
let register_customs (gens : custom list) =
  List.iter register_custom gens

(** Clear all custom generators (useful for testing) *)
let clear_custom_generators () =
  custom_generators := []

(** Get all generators (custom first, then built-in for fallback) *)
let get_all () =
  let custom_as_builtin = List.map custom_to_builtin !custom_generators in
  (* Custom generators take precedence over built-in ones *)
  custom_as_builtin @ !builtin_generators

(** For backward compatibility *)
let generators = builtin_generators

(** Check if a file extension has a registered generator *)
let is_generator_ext ext =
  List.exists (fun gen -> gen.suffix = ext) (get_all ())

(** Check if a filename matches any custom generator *)
let matches_custom_generator filepath =
  let filename = fn_to_string (path_basename filepath) in
  let ext =
    try
      let i = String.rindex filename '.' in
      String.sub filename (i + 1) (String.length filename - i - 1)
    with Not_found -> ""
  in
  List.exists (fun (custom : custom) ->
    match custom.custom_match with
    | Match_suffix s -> s = ext
    | Match_filename f -> f = filename
    | Match_pattern p ->
        (* Simple glob matching - only supports *.ext patterns for now *)
        if String.length p > 2 && p.[0] = '*' && p.[1] = '.' then
          let pattern_ext = String.sub p 2 (String.length p - 2) in
          ext = pattern_ext
        else
          false
    | Match_directory -> Filesystem.is_dir filepath
  ) !custom_generators

(** Get the custom generator that matches a filepath *)
let get_custom_generator filepath =
  let filename = fn_to_string (path_basename filepath) in
  let ext =
    try
      let i = String.rindex filename '.' in
      String.sub filename (i + 1) (String.length filename - i - 1)
    with Not_found -> ""
  in
  try
    Some (List.find (fun (custom : custom) ->
      match custom.custom_match with
      | Match_suffix s -> s = ext
      | Match_filename f -> f = filename
      | Match_pattern p ->
          if String.length p > 2 && p.[0] = '*' && p.[1] = '.' then
            let pattern_ext = String.sub p 2 (String.length p - 2) in
            ext = pattern_ext
          else
            false
      | Match_directory -> Filesystem.is_dir filepath
    ) !custom_generators)
  with Not_found -> None

(** Get generator for filepath based on extension *)
let get_generator fp =
  let ext = Filetype.of_filepath fp in
  let s = match ext with Filetype.FileOther s -> s | _ -> raise (GeneratorNotFound (fp_to_string fp)) in
  try
    List.find (fun gen -> gen.suffix = s) (get_all ())
  with Not_found ->
    raise (GeneratorNotFound (fp_to_string fp))

(** Run generator for source file *)
let run dest src modName =
  verbose Debug "  generator dest = %s src = %s\n%!" (fp_to_string dest) (fp_to_string src);
  let gen = get_generator src in
  let args = gen.commands src dest modName in
  List.iter (fun arg ->
      match Process.run arg with
      | Process.Success (_, warnings,_) -> print_warnings warnings
      | Process.Failure er -> raise (GeneratorFailed er) ) args

(** Run a custom generator with multiple inputs (for generate blocks) *)
let run_custom_multi ~generator_name ~dest ~sources ~extra_args =
  (* Find the custom generator by name *)
  let custom =
    try List.find (fun (g : custom) -> g.custom_name = generator_name) !custom_generators
    with Not_found ->
      (* Try built-in generators by suffix *)
      try
        let builtin = List.find (fun g -> g.suffix = generator_name) !builtin_generators in
        (* Convert built-in to a pseudo-custom for uniform handling *)
        {
          custom_name = generator_name;
          custom_match = Match_suffix builtin.suffix;
          custom_command = "";  (* Not used for built-in *)
          custom_outputs = [];
          custom_module_name = None;
          custom_multi_input = false;
        }
      with Not_found ->
        raise (GeneratorNotFound generator_name)
  in

  (* For custom generators, use the command template *)
  if custom.custom_command <> "" then begin
    let src = match sources with
      | [] -> raise (GeneratorFailed "No source files provided")
      | s :: _ -> s
    in
    let cmd_base = substitute_variables ~src ~dest ~sources custom.custom_command in
    let cmd = match extra_args with
      | None -> cmd_base
      | Some args -> cmd_base ^ " " ^ args
    in
    verbose Debug "  custom generator: %s\n%!" cmd;
    (* Run command through shell to support shell features *)
    let args = ["sh"; "-c"; cmd] in
    match Process.run args with
    | Process.Success (_, warnings, _) -> print_warnings warnings
    | Process.Failure er -> raise (GeneratorFailed er)
  end else begin
    (* For built-in generators, use the standard run function *)
    match sources with
    | [] -> raise (GeneratorFailed "No source files provided")
    | [src] -> run dest src (fn_to_string (path_basename dest))
    | _ -> raise (GeneratorFailed ("Generator " ^ generator_name ^ " does not support multiple inputs"))
  end

(** Get the output files for a custom generator *)
let get_custom_outputs (custom : custom) ~src =
  List.map (fun pattern ->
    fn (substitute_output_pattern ~src pattern)
  ) custom.custom_outputs

(** Check if a generator supports multiple inputs *)
let is_multi_input generator_name =
  try
    let custom = List.find (fun (g : custom) -> g.custom_name = generator_name) !custom_generators in
    custom.custom_multi_input
  with Not_found -> false
