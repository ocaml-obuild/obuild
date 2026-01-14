(** Simple, portable CLI library - Clean implementation *)

open Compat

(* ===== Type Definitions ===== *)

type arg_value =
  | VBool of bool
  | VString of string
  | VInt of int
  | VStrings of string list

type context = {
  command_name : string;
  values : (string, arg_value) Hashtbl.t;
  positionals : string list;
}

type arg_spec = {
  arg_name : string;
  arg_short : char option;
  arg_env : string option;
  arg_kind : [ `Flag | `String of string option * string  (* default, placeholder *)
             | `Int of int option * string | `Strings of string | `Positional of string | `Positionals of string ];
  arg_doc : string;
}

type command_run = context -> unit

type command = {
  cmd_name : string;
  cmd_doc : string;
  cmd_description : string option;
  cmd_args : arg_spec list;
  cmd_run : command_run option;
  cmd_subcommands : command list;
}

type app = {
  app_name : string;
  app_version : string;
  app_doc : string;
  app_description : string option;
  app_global_args : arg_spec list;
  app_on_global_args : (context -> unit) option;
  app_commands : command list;
}

(* ===== Exceptions and Global State ===== *)

exception Parse_error of string
exception Validation_error of string

let exit_code_ref = ref 0
let set_exit_code code = exit_code_ref := code

(* ===== Context Accessors ===== *)

let get_flag ctx name =
  try match Hashtbl.find ctx.values name with VBool b -> b | _ -> false
  with Not_found -> false

let get_string_opt ctx name =
  try match Hashtbl.find ctx.values name with VString s -> Some s | _ -> None
  with Not_found -> None

let get_string ctx name ~default =
  match get_string_opt ctx name with Some s -> s | None -> default

let get_int_opt ctx name =
  try match Hashtbl.find ctx.values name with VInt i -> Some i | _ -> None
  with Not_found -> None

let get_int ctx name ~default =
  match get_int_opt ctx name with Some i -> i | None -> default

let get_strings ctx name =
  try match Hashtbl.find ctx.values name with
      | VStrings l -> l | VString s -> [s] | _ -> []
  with Not_found -> []

let get_positionals ctx = ctx.positionals
let get_command_name ctx = ctx.command_name

(* ===== Command Construction (Functional Builders) ===== *)

let command name ~doc ?description ?(args=[]) ~run () =
  let base_cmd = { cmd_name = name; cmd_doc = doc; cmd_description = description;
    cmd_args = []; cmd_run = Some run; cmd_subcommands = [] } in
  List.fold_left (fun cmd f -> f cmd) base_cmd args

let command_with_subcommands name ~doc ?description ~commands =
  { cmd_name = name; cmd_doc = doc; cmd_description = description;
    cmd_args = []; cmd_run = None; cmd_subcommands = commands }

let app name ~version ~doc ?description ?(global_args=[]) ?on_global_args ~commands () =
  (* Create a temporary command to collect global args *)
  let temp_cmd = { cmd_name = ""; cmd_doc = ""; cmd_description = None;
    cmd_args = []; cmd_run = None; cmd_subcommands = [] } in
  let with_args = List.fold_left (fun cmd f -> f cmd) temp_cmd global_args in
  { app_name = name; app_version = version; app_doc = doc;
    app_description = description; app_global_args = with_args.cmd_args;
    app_on_global_args = on_global_args; app_commands = commands }

(* ===== Argument Builders (Return modified command) ===== *)

let add_arg spec cmd =
  { cmd with cmd_args = spec :: cmd.cmd_args }

let flag name ?short ?env ~doc cmd =
  add_arg { arg_name = name; arg_short = short; arg_env = env;
            arg_kind = `Flag; arg_doc = doc } cmd

let option_string name ?short ?env ?default ?placeholder ~doc cmd =
  let ph = match placeholder with Some p -> p | None -> "STRING" in
  add_arg { arg_name = name; arg_short = short; arg_env = env;
            arg_kind = `String (default, ph); arg_doc = doc } cmd

let option_int name ?short ?env ?default ?placeholder ~doc cmd =
  let ph = match placeholder with Some p -> p | None -> "INT" in
  add_arg { arg_name = name; arg_short = short; arg_env = env;
            arg_kind = `Int (default, ph); arg_doc = doc } cmd

let option_strings name ?short ?env ?placeholder ~doc cmd =
  let ph = match placeholder with Some p -> p | None -> "STRING" in
  add_arg { arg_name = name; arg_short = short; arg_env = env;
            arg_kind = `Strings ph; arg_doc = doc } cmd

let positional name ?placeholder ~doc cmd =
  let ph = match placeholder with Some p -> p | None -> string_uppercase name in
  add_arg { arg_name = name; arg_short = None; arg_env = None;
            arg_kind = `Positional ph; arg_doc = doc } cmd

let positionals name ?placeholder ~doc cmd =
  let ph = match placeholder with Some p -> p | None -> string_uppercase name in
  add_arg { arg_name = name; arg_short = None; arg_env = None;
            arg_kind = `Positionals ph; arg_doc = doc } cmd

(* ===== Standard Flags ===== *)

let version_flag cmd = flag "version" ~doc:"Show version information" cmd
let help_flag cmd = flag "help" ~short:'h' ~doc:"Show this help message" cmd
let verbose_flag cmd = flag "verbose" ~short:'v' ~doc:"Verbose output" cmd
let quiet_flag cmd = flag "quiet" ~short:'q' ~doc:"Quiet mode (errors only)" cmd

(* ===== Error Formatting ===== *)

let format_error msg =
  Printf.sprintf "\027[1;31mError:\027[0m %s" msg

let format_suggestion similar =
  if similar = [] then ""
  else "\n\n\027[1mDid you mean:\027[0m\n  " ^ String.concat "\n  " similar

(* ===== String Utilities ===== *)

let levenshtein_distance s1 s2 =
  let m = String.length s1 and n = String.length s2 in
  if m = 0 then n else if n = 0 then m else
  let d = Array.make_matrix (m + 1) (n + 1) 0 in
  for i = 0 to m do d.(i).(0) <- i done;
  for j = 0 to n do d.(0).(j) <- j done;
  for j = 1 to n do
    for i = 1 to m do
      let cost = if s1.[i-1] = s2.[j-1] then 0 else 1 in
      d.(i).(j) <- min (min (d.(i-1).(j) + 1) (d.(i).(j-1) + 1)) (d.(i-1).(j-1) + cost)
    done
  done;
  d.(m).(n)

let suggest_similar candidates target =
  let scored = List.map (fun c -> (c, levenshtein_distance c target)) candidates in
  let sorted = List.sort (fun (_, d1) (_, d2) -> compare d1 d2) scored in
  let threshold = max 3 ((String.length target) / 2) in
  let rec take n = function
    | [] -> []
    | x :: xs -> if n <= 0 then [] else x :: take (n-1) xs
  in
  take 5 (List.filter (fun (_, d) -> d <= threshold) sorted |> List.map fst)

let suggest_command app name =
  suggest_similar (List.map (fun c -> c.cmd_name) app.app_commands) name

(* ===== Configuration File Support ===== *)

type config = (string, string) Hashtbl.t

let parse_config_line line =
  (* Skip comments and empty lines *)
  let trimmed = String.trim line in
  if String.length trimmed = 0 || trimmed.[0] = '#' then
    None
  else
    try
      let eq_pos = String.index trimmed '=' in
      let key = String.trim (String.sub trimmed 0 eq_pos) in
      let value = String.trim (String.sub trimmed (eq_pos + 1) (String.length trimmed - eq_pos - 1)) in
      Some (key, value)
    with Not_found -> None

let load_config_file path =
  let config = Hashtbl.create 16 in
  if Sys.file_exists path then (
    try
      let ic = open_in path in
      try
        while true do
          let line = input_line ic in
          match parse_config_line line with
          | Some (key, value) -> Hashtbl.replace config key value
          | None -> ()
        done;
        config
      with End_of_file ->
        close_in ic;
        config
    with Sys_error _ -> config
  ) else
    config

let load_config ?(paths=[]) () =
  let default_paths =
    if paths = [] then
      let home = try Sys.getenv "HOME" with Not_found -> "" in
      let user_config = if home <> "" then [Filename.concat home ".obuildrc"] else [] in
      let project_config = [".obuildrc"] in
      user_config @ project_config
    else
      paths
  in
  let config = Hashtbl.create 16 in
  List.iter (fun path ->
    let file_config = load_config_file path in
    Hashtbl.iter (fun k v -> Hashtbl.replace config k v) file_config
  ) default_paths;
  config

let config_get_string config key =
  try Some (Hashtbl.find config key)
  with Not_found -> None

let config_get_int config key =
  try Some (int_of_string (Hashtbl.find config key))
  with Not_found | Failure _ -> None

let config_get_bool config key =
  try
    let value = String.lowercase_ascii (Hashtbl.find config key) in
    match value with
    | "true" | "yes" | "1" | "on" -> Some true
    | "false" | "no" | "0" | "off" -> Some false
    | _ -> None
  with Not_found -> None

(* ===== Shell Completion Generation ===== *)

let generate_bash_completion app =
  let commands = String.concat " " (List.map (fun c -> c.cmd_name) app.app_commands) in
  Printf.sprintf {|# Bash completion for %s
_%s_completions() {
    local cur prev
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    # Complete commands
    if [ $COMP_CWORD -eq 1 ]; then
        COMPREPLY=( $(compgen -W "%s" -- "$cur") )
        return 0
    fi

    # Complete global flags
    local flags="--help --version --verbose --quiet --debug --color --strict"
    COMPREPLY=( $(compgen -W "$flags" -- "$cur") )
    return 0
}

complete -F _%s_completions %s
|} app.app_name app.app_name commands app.app_name app.app_name

let generate_zsh_completion app =
  let cmd_list = String.concat "\n    " (List.map (fun c ->
    Printf.sprintf "'%s:%s'" c.cmd_name c.cmd_doc
  ) app.app_commands) in
  Printf.sprintf {|#compdef %s

_%s() {
  local -a commands
  commands=(
    %s
  )

  _arguments -C \
    '(- 1 *)'{-h,--help}'[Show help message]' \
    '(- 1 *)--version[Show version information]' \
    {-v,--verbose}'[Verbose output]' \
    {-q,--quiet}'[Quiet mode]' \
    '--color[Enable colored output]' \
    '--strict[Enable strict mode]' \
    '1: :->cmds' \
    '*:: :->args' && return 0

  case "$state" in
    cmds)
      _describe -t commands 'command' commands
      ;;
  esac
}

_%s "$@"
|} app.app_name app.app_name cmd_list app.app_name

let generate_fish_completion app =
  let completions = String.concat "\n" (List.map (fun c ->
    Printf.sprintf "complete -c %s -n '__fish_use_subcommand' -a %s -d '%s'"
      app.app_name c.cmd_name c.cmd_doc
  ) app.app_commands) in
  Printf.sprintf {|# Fish completion for %s

# Global options
complete -c %s -s h -l help -d 'Show help message'
complete -c %s -l version -d 'Show version information'
complete -c %s -s v -l verbose -d 'Verbose output'
complete -c %s -s q -l quiet -d 'Quiet mode'
complete -c %s -l color -d 'Enable colored output'
complete -c %s -l strict -d 'Enable strict mode'

# Commands
%s
|} app.app_name app.app_name app.app_name app.app_name app.app_name
   app.app_name app.app_name completions

(* ===== Help Generation ===== *)

let print_usage_line chan app_name cmd_name =
  Printf.fprintf chan "Usage: %s" app_name;
  (match cmd_name with Some n -> Printf.fprintf chan " %s" n | None -> ());
  Printf.fprintf chan " [OPTIONS]";
  Printf.fprintf chan "\n"

let print_arg_help chan spec =
  let short_str = match spec.arg_short with
    | Some c -> Printf.sprintf "-%c, " c
    | None -> "    "
  in
  let long_name = "--" ^ spec.arg_name in
  let placeholder = match spec.arg_kind with
    | `String (_, ph) | `Int (_, ph) | `Strings ph | `Positional ph | `Positionals ph -> " " ^ ph
    | `Flag -> ""
  in
  Printf.fprintf chan "  %s%s%s\n" short_str long_name placeholder;
  Printf.fprintf chan "      %s\n" spec.arg_doc

let print_help app cmd_opt =
  let chan = stdout in
  (match cmd_opt with
   | None ->
       Printf.fprintf chan "%s - %s\n" app.app_name app.app_doc;
       Printf.fprintf chan "Version: %s\n\n" app.app_version;
       (match app.app_description with Some d -> Printf.fprintf chan "%s\n\n" d | None -> ());
       print_usage_line chan app.app_name None;
       if app.app_global_args <> [] then (
         Printf.fprintf chan "\nGlobal Options:\n";
         List.iter (print_arg_help chan) (List.rev app.app_global_args)
       );
       Printf.fprintf chan "\nCommands:\n";
       List.iter (fun cmd ->
         Printf.fprintf chan "  %-20s %s\n" cmd.cmd_name cmd.cmd_doc
       ) app.app_commands;
       Printf.fprintf chan "\nRun '%s COMMAND -h' for command-specific help.\n" app.app_name
   | Some cmd ->
       Printf.fprintf chan "%s %s - %s\n\n" app.app_name cmd.cmd_name cmd.cmd_doc;
       (match cmd.cmd_description with Some d -> Printf.fprintf chan "%s\n\n" d | None -> ());
       print_usage_line chan app.app_name (Some cmd.cmd_name);
       if cmd.cmd_args <> [] then (
         Printf.fprintf chan "\nOptions:\n";
         List.iter (print_arg_help chan) (List.rev cmd.cmd_args)
       );
       if cmd.cmd_subcommands <> [] then (
         Printf.fprintf chan "\nSubcommands:\n";
         List.iter (fun subcmd ->
           Printf.fprintf chan "  %-20s %s\n" subcmd.cmd_name subcmd.cmd_doc
         ) cmd.cmd_subcommands
       )
  );
  flush chan

(* ===== Argument Parsing ===== *)

let parse_args ?(stop_at_positional=false) specs argv start_idx =
  let values = Hashtbl.create 16 in
  let positionals = ref [] in
  let idx = ref start_idx in
  let len = Array.length argv in
  let stopped = ref false in

  (* Initialize defaults and env vars *)
  List.iter (fun spec ->
    match spec.arg_kind with
    | `String (Some def, _) -> Hashtbl.add values spec.arg_name (VString def)
    | `Int (Some def, _) -> Hashtbl.add values spec.arg_name (VInt def)
    | `Flag -> Hashtbl.add values spec.arg_name (VBool false)
    | _ -> ()
  ) specs;

  while !idx < len && not !stopped do
    let arg = argv.(!idx) in
    if String.length arg > 0 && arg.[0] = '-' then (
      (* Parse option *)
      let is_long = String.length arg > 1 && arg.[1] = '-' in
      let opt_name, opt_val =
        if is_long then
          (* Long option: --name or --name=value *)
          let name_part = String.sub arg 2 (String.length arg - 2) in
          (try
            let eq_pos = String.index name_part '=' in
            (String.sub name_part 0 eq_pos, Some (String.sub name_part (eq_pos+1) (String.length name_part - eq_pos - 1)))
           with Not_found -> (name_part, None))
        else
          (* Short option: -n or -nvalue *)
          if String.length arg < 2 then raise (Parse_error "Invalid option")
          else (String.make 1 arg.[1], if String.length arg > 2 then Some (String.sub arg 2 (String.length arg - 2)) else None)
      in
      (* Find matching spec *)
      let spec_opt =
        SafeList.find_opt (fun s ->
          if is_long then s.arg_name = opt_name
          else match s.arg_short with Some c -> String.make 1 c = opt_name | None -> false
        ) specs
      in
      (match spec_opt with
       | None -> raise (Parse_error ("Unknown option: " ^ arg))
       | Some spec ->
           (match spec.arg_kind with
            | `Flag ->
                Hashtbl.replace values spec.arg_name (VBool true);
                incr idx
            | `String _ | `Int _ ->
                let value =
                  match opt_val with
                  | Some v -> v
                  | None ->
                      incr idx;
                      if !idx >= len then raise (Parse_error (spec.arg_name ^ " requires a value"));
                      argv.(!idx)
                in
                (match spec.arg_kind with
                 | `String _ -> Hashtbl.replace values spec.arg_name (VString value)
                 | `Int _ ->
                     (try Hashtbl.replace values spec.arg_name (VInt (int_of_string value))
                      with Failure _ -> raise (Parse_error (spec.arg_name ^ " requires an integer")))
                 | _ -> ());
                incr idx
            | `Strings _ ->
                let value =
                  match opt_val with
                  | Some v -> v
                  | None ->
                      incr idx;
                      if !idx >= len then raise (Parse_error (spec.arg_name ^ " requires a value"));
                      argv.(!idx)
                in
                let existing = try match Hashtbl.find values spec.arg_name with VStrings l -> l | _ -> [] with Not_found -> [] in
                Hashtbl.replace values spec.arg_name (VStrings (existing @ [value]));
                incr idx
            | _ -> incr idx
           )
      )
    ) else (
      (* Positional argument *)
      positionals := !positionals @ [arg];
      incr idx;
      if stop_at_positional then stopped := true
    )
  done;

  (* Collect remaining args if we stopped early *)
  let remaining = ref [] in
  while !idx < len do
    remaining := !remaining @ [argv.(!idx)];
    incr idx
  done;

  (values, List.rev !positionals @ !remaining)

(* ===== Main Execution ===== *)

let run_internal argv app =
  if Array.length argv < 2 then (
    print_help app None;
    raise (Parse_error "No command specified")
  );

  (* Parse global args - stop at first positional (command name) *)
  let global_vals, remaining = parse_args ~stop_at_positional:true app.app_global_args argv 1 in

  (* Check for global --help or --version *)
  (try
    let global_ctx = { command_name = ""; values = global_vals; positionals = [] } in
    if get_flag global_ctx "help" then (
      print_help app None;
      exit 0
    );
    if get_flag global_ctx "version" then (
      Printf.printf "%s %s\n" app.app_name app.app_version;
      exit 0
    )
  with _ -> ());

  (* Invoke global args callback if provided *)
  (match app.app_on_global_args with
   | Some f -> f { command_name = ""; values = global_vals; positionals = [] }
   | None -> ());

  if List.length remaining = 0 then (
    print_help app None;
    raise (Parse_error "No command specified")
  );

  let cmd_name = List.hd remaining in
  let cmd_args = List.tl remaining in

  (* Find command *)
  let cmd_opt = SafeList.find_opt (fun c -> c.cmd_name = cmd_name) app.app_commands in
  match cmd_opt with
  | None ->
      let suggestions = suggest_command app cmd_name in
      let msg = Printf.sprintf "Unknown command '%s'" cmd_name in
      let msg = msg ^ format_suggestion suggestions in
      raise (Parse_error msg)
  | Some cmd ->
      (* Parse command args *)
      let cmd_vals, cmd_positionals = parse_args cmd.cmd_args (Array.of_list cmd_args) 0 in
      let ctx = { command_name = cmd.cmd_name; values = cmd_vals; positionals = cmd_positionals } in

      (* Check for command --help *)
      if get_flag ctx "help" then (
        print_help app (Some cmd);
        exit 0
      );

      (* Run command or route to subcommand *)
      (match cmd.cmd_run with
       | Some run -> run ctx
       | None ->
           if cmd.cmd_subcommands = [] then
             raise (Parse_error "Command has no implementation")
           else (
             (* Handle subcommands *)
             if List.length cmd_positionals = 0 then (
               print_help app (Some cmd);
               raise (Parse_error "Subcommand required")
             );

             let subcmd_name = List.hd cmd_positionals in
             let subcmd_args = List.tl cmd_positionals in

             (* Find subcommand *)
             let subcmd_opt = SafeList.find_opt (fun c -> c.cmd_name = subcmd_name) cmd.cmd_subcommands in
             match subcmd_opt with
             | None ->
                 let msg = Printf.sprintf "Unknown subcommand '%s'" subcmd_name in
                 raise (Parse_error msg)
             | Some subcmd ->
                 (* Parse subcommand args *)
                 let subcmd_vals, subcmd_positionals = parse_args subcmd.cmd_args (Array.of_list subcmd_args) 0 in
                 let subcmd_ctx = { command_name = subcmd.cmd_name; values = subcmd_vals; positionals = subcmd_positionals } in

                 (* Check for subcommand --help *)
                 if get_flag subcmd_ctx "help" then (
                   (* TODO: print subcommand help - for now just print command help *)
                   print_help app (Some subcmd);
                   exit 0
                 );

                 (* Run subcommand *)
                 (match subcmd.cmd_run with
                  | Some run -> run subcmd_ctx
                  | None -> raise (Parse_error "Subcommand has no implementation"))
           )
      )

let run ?(argv=Sys.argv) app =
  try
    run_internal argv app;
    exit !exit_code_ref
  with
  | Parse_error msg ->
      Printf.eprintf "%s\n" (format_error msg);
      exit 1
  | Validation_error msg ->
      Printf.eprintf "%s\n" (format_error ("Validation failed: " ^ msg));
      exit 1
  | Failure msg ->
      Printf.eprintf "%s\n" (format_error msg);
      exit 1

let run_result ?(argv=Sys.argv) app =
  try
    run_internal argv app;
    Ok ()
  with
  | Parse_error msg -> Error ("Parse error: " ^ msg)
  | Validation_error msg -> Error ("Validation error: " ^ msg)
  | Failure msg -> Error msg
  | exn -> Error (Printexc.to_string exn)

(* Apply config defaults to argument specs *)
let apply_config_to_specs config specs =
  List.map (fun spec ->
    match spec.arg_kind with
    | `String (_, ph) ->
        (match config_get_string config spec.arg_name with
         | Some value -> { spec with arg_kind = `String (Some value, ph) }
         | None -> spec)
    | `Int (_, ph) ->
        (match config_get_int config spec.arg_name with
         | Some value -> { spec with arg_kind = `Int (Some value, ph) }
         | None -> spec)
    | `Flag ->
        (match config_get_bool config spec.arg_name with
         | Some true -> spec  (* Can't set flag default to true in current system, would need VBool in hashtable *)
         | _ -> spec)
    | _ -> spec
  ) specs

let run_with_config ?(argv=Sys.argv) ?config app =
  let app_with_config =
    match config with
    | None -> app
    | Some cfg ->
        (* Apply config to global args and all command args *)
        let new_global_args = apply_config_to_specs cfg app.app_global_args in
        let new_commands = List.map (fun cmd ->
          { cmd with cmd_args = apply_config_to_specs cfg cmd.cmd_args }
        ) app.app_commands in
        { app with app_global_args = new_global_args; app_commands = new_commands }
  in
  run ~argv app_with_config
