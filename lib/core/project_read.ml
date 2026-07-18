(** New parser integration for Project reading

    This module provides an alternative to Project.read that uses the new parser (obuild_lexer ->
    obuild_parser -> obuild_validate).

    It's in a separate module to avoid cyclic dependencies between Project and Obuild_validate. *)

open Filepath

(** Convert Project.Generator.t to Generators.custom *)
let convert_generator_to_custom (gen : Project.Generator.t) : Generators.custom =
  {
    Generators.custom_name = gen.Project.Generator.name;
    custom_suffix = gen.Project.Generator.suffix;
    custom_command = gen.Project.Generator.command;
    custom_outputs = gen.Project.Generator.outputs;
    custom_module_name = gen.Project.Generator.module_name;
  }

(** Register custom generators from project *)
let register_generators proj =
  (* Clear any previously registered custom generators *)
  Generators.clear_custom_generators ();
  (* Register new ones *)
  List.iter (fun gen ->
    Generators.register_custom (convert_generator_to_custom gen)
  ) proj.Project.generators

(* ===== Vendored sub-projects =====

   A `vendor-dirs:` entry either is itself a vendored obuild project directory
   or contains vendored project directories (e.g. vendor/*/x.obuild — the
   layout produced by git submodules).  Only the *libraries* of a vendored
   project are merged into the parent, re-rooted and made non-installable:
   vendoring provides code to build against, not programs or packages.
   Their build-deps resolve like any others: against project-internal
   libraries first (including other vendored ones), then findlib. *)

(* a vendored project directory must contain exactly one .obuild file *)
let find_vendored_project_file dir =
  try
    let entries = Array.to_list (Sys.readdir (fp_to_string dir)) in
    match List.filter (fun f -> Filename.check_suffix f ".obuild") entries with
    | [ f ] -> Some (dir </> fn f)
    | _ -> None
  with Sys_error _ -> None

let reroot prefix p = if p = current_dir then prefix else prefix <//> p

let reroot_target prefix (t : Target.target) =
  let obits = t.Target.target_obits in
  let cbits = t.Target.target_cbits in
  {
    t with
    Target.target_obits =
      { obits with Target.target_srcdir = List.map (reroot prefix) obits.Target.target_srcdir };
    target_cbits = { cbits with Target.target_cdir = reroot prefix cbits.Target.target_cdir };
    target_generates =
      List.map
        (fun (g : Target.target_generate) ->
          { g with Target.generate_from = List.map (reroot prefix) g.Target.generate_from })
        t.Target.target_generates;
    (* vendored code is built into the parent's artifacts, not installed *)
    target_installable = Target.BoolConst false;
  }

let rec reroot_library prefix (lib : Project.Library.t) =
  {
    lib with
    Project.Library.target = reroot_target prefix lib.Project.Library.target;
    subs = List.map (reroot_library prefix) lib.Project.Library.subs;
  }

let parse_project_file path =
  try Obuild_validate.parse_and_convert_file (fp_to_string path) with
  | Obuild_validate.Validation_error (loc, msg) ->
      raise
        (Project.InvalidConfFile
           (Printf.sprintf "%s: %d:%d: %s" (fp_to_string path) loc.Location.line loc.Location.col
              msg))
  | Obuild_parser.Parser_error (loc, msg) ->
      raise
        (Project.InvalidConfFile
           (Printf.sprintf "%s: %d:%d: %s" (fp_to_string path) loc.Location.line loc.Location.col
              msg))

(* returns (libraries, custom generators) of the vendored project at [dir],
   including its own vendored projects, recursively.  [visited] guards
   against cycles and double-vendoring of the same project. *)
let rec collect_vendored visited dir =
  match find_vendored_project_file dir with
  | None -> ([], [])
  | Some path ->
      let key = fp_to_string path in
      if Hashtbl.mem visited key then
        ([], [])
      else begin
        Hashtbl.add visited key ();
        let proj = parse_project_file path in
        let libs = List.map (reroot_library dir) proj.Project.libs in
        let nested =
          List.map (fun d -> scan_vendor_dir visited (reroot dir d)) proj.Project.vendor_dirs
        in
        ( libs @ List.concat (List.map fst nested),
          proj.Project.generators @ List.concat (List.map snd nested) )
      end

and scan_vendor_dir visited dir =
  if find_vendored_project_file dir <> None then
    collect_vendored visited dir
  else
    let subdirs =
      try
        List.filter Filesystem.is_dir
          (List.map
             (fun e -> dir </> fn e)
             (List.fast_sort String.compare (Array.to_list (Sys.readdir (fp_to_string dir)))))
      with Sys_error _ -> []
    in
    let results = List.map (collect_vendored visited) subdirs in
    (List.concat (List.map fst results), List.concat (List.map snd results))

let merge_vendored proj =
  match proj.Project.vendor_dirs with
  | [] -> proj
  | dirs ->
      let visited = Hashtbl.create 4 in
      let results = List.map (scan_vendor_dir visited) dirs in
      let vlibs = List.concat (List.map fst results) in
      let vgens = List.concat (List.map snd results) in
      let seen = Hashtbl.create 8 in
      List.iter
        (fun (l : Project.Library.t) ->
          let n = Libname.to_string l.Project.Library.name in
          if Hashtbl.mem seen n then
            raise
              (Project.InvalidConfFile
                 (Printf.sprintf
                    "library %s is defined more than once (vendored library name collision)" n));
          Hashtbl.add seen n ())
        (proj.Project.libs @ vlibs);
      {
        proj with
        Project.libs = proj.Project.libs @ vlibs;
        Project.generators = proj.Project.generators @ vgens;
      }

(** Read project file using the new parser *)
let read () =
  let path = Project.findPath () in
  let proj = parse_project_file path in
  (* merge vendored sub-project libraries as internal targets *)
  let proj = merge_vendored proj in
  (* Apply ocaml_extra_args side effect *)
  (match proj.Project.ocaml_extra_args with
  | Some args -> Gconf.gconf.Gconf.ocaml_extra_args <- args
  | None -> ());
  (* Register custom generators *)
  register_generators proj;
  (* Validate file existence *)
  Project.check proj;
  proj
