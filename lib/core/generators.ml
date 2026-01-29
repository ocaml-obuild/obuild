open Filepath
open Helper
open Gconf

exception GeneratorFailed of string
exception GeneratorNotFound of string

(** Internal generator representation for build system integration *)
type t = {
  suffix : string;
  modname : (Modname.t -> Modname.t);
  commands : (filepath -> filepath -> string -> string list list);
  generated_files : (filename -> string -> filename);
}

(** Custom generator definition from .obuild file *)
type custom = {
  custom_name : string;                   (** Generator name for reference *)
  custom_suffix : string option;          (** File extension for automatic detection *)
  custom_command : string;                (** Command template with variables *)
  custom_outputs : string list;           (** Output file patterns *)
  custom_module_name : string option;     (** Module name pattern if different from base *)
}

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

(** Convert custom generator to internal type for build system *)
let custom_to_builtin (custom : custom) : t =
  let suffix = match custom.custom_suffix with
    | Some s -> s
    | None -> ""  (* No suffix means generate-block-only *)
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

(** Get all generators with suffixes (for automatic detection) *)
let get_all () =
  let custom_as_builtin = List.map custom_to_builtin !custom_generators in
  (* Only include generators with non-empty suffix for automatic detection *)
  List.filter (fun gen -> gen.suffix <> "") custom_as_builtin

(** Check if a file extension has a registered generator *)
let is_generator_ext ext =
  List.exists (fun gen -> gen.suffix = ext) (get_all ())

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

(** Find a custom generator by name *)
let find_generator_by_name name =
  try Some (List.find (fun (g : custom) -> g.custom_name = name) !custom_generators)
  with Not_found -> None

(** Run a generator with multiple inputs (for generate blocks) *)
let run_custom_multi ~generator_name ~dest ~sources ~extra_args =
  (* Find the custom generator by name *)
  let custom =
    match find_generator_by_name generator_name with
    | Some g -> g
    | None -> raise (GeneratorNotFound generator_name)
  in

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

(** Get the output files for a custom generator *)
let get_custom_outputs (custom : custom) ~src =
  List.map (fun pattern ->
    fn (substitute_output_pattern ~src pattern)
  ) custom.custom_outputs
