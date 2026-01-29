open Fugue
(** Dependency Analysis and Compilation State Preparation

    This module analyzes dependencies and creates the compilation state for build targets. It
    constructs two separate DAG structures that serve different purposes in the build system.

    = Two DAG Architecture =

    Obuild uses two distinct Directed Acyclic Graphs for dependency tracking:

    1. Files DAG (filesDag):
    - Purpose: Track file-level dependencies for incremental builds
    - Nodes: Individual files (.ml, .mli, .c, .h, .cmi, .cmo, .cmx, .o)
    - Edges: "file A depends on file B" (based on content dependencies)
    - Usage: Modification time checking to determine what needs rebuilding
    - Example: bar.cmo → [bar.ml, bar.cmi, foo.cmi] (bar.cmo depends on these files; if any change,
      recompile)

    2. Steps DAG (stepsDag / compilation_dag):
    - Purpose: Define task execution order for parallel builds
    - Nodes: Compilation tasks (CompileModule, CompileInterface, CompileC, LinkTarget)
    - Edges: "task A must complete before task B" (ordering constraints)
    - Usage: Topological sort for parallel scheduling, respecting dependencies
    - Example: CompileModule(Bar) → CompileInterface(Foo) (must compile Foo's interface before Bar's
      implementation)

    = Why Two DAGs? =

    The separation serves distinct build system needs:
    - Files DAG answers: "What changed?" (incremental build detection)
    - Steps DAG answers: "What order?" (parallel execution scheduling)

    Example: When foo.ml changes but foo.mli doesn't:
    - Files DAG: bar.cmo doesn't depend on foo.cmo (in bytecode), so bar doesn't rebuild
    - Steps DAG: CompileModule(Bar) still depends on CompileInterface(Foo) for ordering

    = Key Implementation Notes =

    - File dependencies are checked via mtime comparison (see build.ml:check_destination_valid)
    - Step dependencies ensure parallel builds respect compilation order
    - C object files are only in Files DAG (added in get_dags around line 518)
    - OCaml module dependencies populate both DAGs (around lines 436-494)

    = Historical Bug Fix =

    Prior to Phase 4 debugging, bytecode .cmo files incorrectly depended on other .cmo files in the
    Files DAG. This caused unnecessary rebuilds. Fixed in build.ml:230-237 to only add .cmx
    dependencies in Native mode, while bytecode depends only on .cmi files. *)

open Filepath
open Analyze
open Types
open Helper
open Gconf
open Target
open Dependencies
open Prepare_types

(* Re-export types from Prepare_types for backwards compatibility *)
type use_thread_flag = Prepare_types.use_thread_flag =
  | NoThread
  | WithThread

type thread_type = Prepare_types.thread_type =
  | VMThread
  | PosixThread
  | DefaultThread
  | NoThreads

type ocaml_file_type = Prepare_types.ocaml_file_type =
  | GeneratedModule
  | SimpleModule

module Module = Prepare_types.Module

(* Re-export types from Prepare_types *)
type build_state = Prepare_types.build_state = { bstate_config : Analyze.project_config }

type dir_spec = Prepare_types.dir_spec = {
  src_dir : Filepath.filepath;
  dst_dir : Filepath.filepath;
  include_dirs : Filepath.filepath list;
}

type compile_step = Prepare_types.compile_step =
  | CompileModule of Hier.t
  | CompileInterface of Hier.t
  | CompileDirectory of Hier.t
  | CompileC of Filepath.filename
  | GenerateCstubsTypes of Libname.t
  | GenerateCstubsFunctions of Libname.t
  | CompileCstubsC of Libname.t
  | RunGenerateBlock of Target.target_generate
  | LinkTarget of Target.target
  | CheckTarget of Target.target

type compilation_state = Prepare_types.compilation_state = {
  compilation_modules : (Hier.t, Module.t) Hashtbl.t;
  compilation_csources : Filepath.filename list;
  compilation_dag : compile_step Dag.t;
  compilation_pp : Pp.t;
  compilation_filesdag : Filetype.id Dag.t;
  compilation_builddir_c : Filepath.filepath;
  compilation_builddir_ml : Types.ocaml_compilation_option -> Filepath.filepath;
  compilation_include_paths : Types.ocaml_compilation_option -> Hier.t -> Filepath.filepath list;
  compilation_linking_paths : Filepath.filepath list;
  compilation_linking_paths_d : Filepath.filepath list;
  compilation_linking_paths_p : Filepath.filepath list;
  compilation_c_include_paths : Filepath.filepath list;
  compilation_c_linking_paths : Filepath.filepath list;
}

let string_of_compile_step = Prepare_types.string_of_compile_step
let init project = { bstate_config = project }

let get_compilation_order cstate =
  let filter_modules t : Hier.t option =
    match t with
    | CompileC _ | CompileInterface _ | LinkTarget _ | CheckTarget _ -> None
    | GenerateCstubsTypes _ | GenerateCstubsFunctions _ | CompileCstubsC _ | RunGenerateBlock _ -> None
    | CompileDirectory m | CompileModule m -> if Hier.lvl m = 0 then Some m else None
  in
  list_filter_map filter_modules (Dagutils.linearize cstate.compilation_dag)

(* PPX/Syntax preprocessing functions moved to Ppx_resolver module *)

(** Helper: Resolve PPX flags for a specific module

    This complex logic handles PPX dependencies and options, ensuring only one PPX is used per
    module and that ppxopt arguments match.

    @param bstate build state
    @param target current target
    @return list of PPX flags to pass to the compiler *)
let resolve_module_ppx_flags bstate target =
  let full_path include_path name =
    match name.[0] with
    | '.' -> fp_to_string include_path ^ "/" ^ name
    | _ -> name
  in
  let stdlib = fp (get_ocaml_config_key "standard_library" bstate.bstate_config) in
  let get_ppx_ppxopt fpath meta libname =
    let includePath = Meta.get_include_dir stdlib (fpath, meta) in
    let pkg = Meta.Pkg.find libname.Libname.subnames meta in
    let ppx = pkg.Meta.Pkg.ppx in
    let ppxopt = pkg.Meta.Pkg.ppxopt in
    (includePath, ppx, ppxopt)
  in

  let target_deps = get_all_builddeps target in
  let dag = bstate.bstate_config.project_pkgdeps_dag in
  let deps_lists =
    list_filter_map
      (fun (l, _) ->
        let dag_dep = Analyze.Dependency l in
        if Dag.exists_node dag_dep dag then begin
          let children = Dag.get_children_full dag dag_dep in
          let deps =
            list_filter_map
              (fun d ->
                match d with
                | Analyze.Target _ -> None
                | Analyze.Dependency l -> Some l)
              children
          in
          let uniq_deps = list_uniq deps in
          Some (l :: uniq_deps)
        end
        else
          None)
      target_deps
  in
  let ppx_list =
    List.map
      (fun l ->
        let ppxs, ppxopts =
          List.fold_left
            (fun (ppxs, ppxopts) d ->
              match Metacache.find d.Libname.main_name with
              | None -> (ppxs, ppxopts)
              | Some (fpath, meta) ->
                  let includePath, ppx, ppxopt = get_ppx_ppxopt fpath meta d in
                  let ppxs_ =
                    match ppx with
                    | None -> ppxs
                    | Some (_, s) -> (includePath, s, d) :: ppxs
                  in
                  let ppxopts_ =
                    ppxopts
                    @ List.map
                        (fun (_, s) ->
                          let ppxargs = String_utils.split ',' s in
                          (includePath, ppxargs))
                        ppxopt
                  in
                  (ppxs_, ppxopts_))
            ([], []) (List.rev l)
        in
        let ppxs = list_uniq ppxs in
        if List.length ppxs > 1 then
          failwith ("More than 1 ppx " ^ String.concat ", " (List.map (fun (_, s, _) -> s) ppxs));
        if List.length ppxs = 0 then
          []
        else
          let includePath, ppx_name, ppx_lib = List.hd ppxs in
          List.iter
            (fun (_, ss) ->
              let res = Libname.of_string (List.hd ss) = ppx_lib in
              if not res then
                failwith ("Different ppx " ^ ppx_name ^ " <> " ^ List.hd ss))
            ppxopts;
          full_path includePath ppx_name
          :: List.map
               (fun (includePath, args) ->
                 String.concat " " (List.map (fun a -> full_path includePath a) (List.tl args)))
               ppxopts)
      deps_lists
  in
  let ppx_list = no_empty [] ppx_list in
  List.flatten (List.map (fun l -> [ "-ppx"; String.concat " " l ]) ppx_list)

(** Helper: Analyze module dependencies using ocamldep

    Runs ocamldep on the source file, then categorizes dependencies into:
    - Internal dependencies (within the same project/directory)
    - External dependencies (from other packages)

    Also detects thread library usage.

    @param srcFile source file to analyze
    @param hier module hierarchy
    @param pp preprocessor settings
    @param file_search_paths search paths for finding modules
    @return (internal_deps, external_deps, use_thread_flag) *)
let analyze_module_dependencies srcFile hier pp file_search_paths =
  let dopt = { dep_includes = file_search_paths hier; dep_pp = pp } in
  let allDeps =
    match run_ocamldep dopt srcFile with
    | [] -> raise Module.DependencyNoOutput
    | ml :: mli :: _ -> list_uniq (ml @ mli)
    | x :: _ -> x
  in
  verbose Debug "  %s depends on %s\n%!" (Hier.to_string hier)
    (String.concat "," (List.map Modname.to_string allDeps));

  (* Partition dependencies into internal (same directory) vs external *)
  let cwdDepsInDir, otherDeps =
    List.partition
      (fun dep ->
        try
          let entry = Hier.get_file_entry (Hier.of_modname dep) (file_search_paths hier) in
          match entry with
          | Hier.DirectoryEntry (p, _) | Hier.FileEntry (p, _) | Hier.GeneratedFileEntry (p, _, _)
            -> List.mem p (file_search_paths hier)
        with Not_found -> false)
      allDeps
  in

  verbose Debug "  %s internally depends on %s\n%!" (Hier.to_string hier)
    (String.concat "," (List.map Modname.to_string cwdDepsInDir));

  (* Detect thread library usage *)
  let use_thread =
    if
      List.mem (Modname.wrap "Thread") otherDeps
      || List.mem (Modname.wrap "Condition") otherDeps
      || List.mem (Modname.wrap "Mutex") otherDeps
    then
      WithThread
    else
      NoThread
  in

  (* Convert internal deps to Hier.t with proper parent context *)
  let cwdDeps =
    List.map
      (fun x -> maybe (Hier.make [ x ]) (fun z -> Hier.append z x) (Hier.parent hier))
      cwdDepsInDir
  in

  (* Check for self-dependency *)
  if List.mem hier cwdDeps then
    raise (Module.DependsItself hier);

  (cwdDeps, otherDeps, use_thread)

(** Helper: Discover modules within a directory

    When a module is represented as a directory, this function scans the directory to find all
    sub-modules (both files and subdirectories).

    @param srcDir directory to scan
    @param hier parent module hierarchy
    @return Module.t descriptor for the directory *)
let discover_directory_modules srcDir hier =
  let modules =
    Filesystem.list_dir_pred_map
      (fun f ->
        let fp = srcDir </> f in
        if Filesystem.is_dir fp then (* Avoid directories like .git/.svn etc. *)
          if not (Modname.string_all Modname.char_is_valid_modchar (fn_to_string f)) then
            None
          else
            Some (Modname.of_directory f)
        else
          match
            Filetype.of_filepath fp
          with
          | Filetype.FileML -> Some (Modname.of_filename f)
          | Filetype.FileMLI ->
              if Filesystem.exists (srcDir </> (chop_extension f <.> "ml")) then
                None (* Skip .mli if corresponding .ml exists *)
              else
                Some (Modname.of_filename f) (* Lonely .mli *)
          | Filetype.FileOther s ->
              if Generators.is_generator_ext s then
                Some (Modname.of_filename f)
              else
                None
          | _ -> None)
      srcDir
  in
  Module.make_dir current_dir (List.map (fun m -> Hier.append hier m) modules)

(** Helper: Compute all dependency paths for compilation and linking

    Calculates include paths and linking paths for both OCaml and C compilation, handling internal
    and system dependencies, and different compilation modes (normal, debug, profiling).

    @param bstate build state
    @param target current target
    @return
      tuple of (depIncludePaths, depIncludePathsD, depIncludePathsP, depLinkingPaths,
      cdepsIncludePaths, cCamlIncludePath) *)
let compute_dependency_paths bstate target =
  let conf = bstate.bstate_config in
  let stdlib = fp (get_ocaml_config_key "standard_library" conf) in
  let cbits = target.target_cbits in

  (* Get package dependencies and partition into internal vs system *)
  let depPkgs = Analyze.get_pkg_deps target conf in
  let depsInternal, depsSystem =
    List.partition
      (fun dep ->
        match Hashtbl.find conf.project_dep_data dep with
        | Internal -> true
        | _ -> false)
      depPkgs
  in

  (* Compute include paths for internal dependencies *)
  let depIncPathInter =
    List.map (fun dep -> Dist.get_build_exn (Dist.Target (Name.Lib dep))) depsInternal
  in

  (* Compute include paths for system dependencies *)
  let depIncPathSystem =
    List.map
      (fun dep ->
        let path, rootPkg = Metacache.get_from_cache dep in
        Meta.get_include_dir_with_subpath stdlib (path, rootPkg) dep.Libname.subnames)
      depsSystem
  in

  (* Combine and compute variants for different compilation modes *)
  let depIncludePaths = depIncPathInter @ depIncPathSystem in
  let depIncludePathsD =
    List.map (fun fp -> fp </> fn "opt-d") depIncPathInter @ depIncPathSystem
  in
  let depIncludePathsP =
    List.map (fun fp -> fp </> fn "opt-p") depIncPathInter @ depIncPathSystem
  in

  (* Compute linking paths *)
  let depLinkingPaths =
    List.map
      (fun dep ->
        match Hashtbl.find conf.project_dep_data dep with
        | Internal -> Dist.get_build_exn (Dist.Target (Name.Lib dep))
        | System ->
            let path, rootPkg = Metacache.get_from_cache dep in
            Meta.get_include_dir_with_subpath stdlib (path, rootPkg) dep.Libname.subnames)
      depPkgs
  in

  (* Compute C include paths *)
  let cdepsIncludePaths : filepath list =
    cbits.target_clibpaths
    @ List.concat
        (List.map
           (fun (cpkg, _) ->
             (Hashtbl.find bstate.bstate_config.project_cpkgs cpkg).cpkg_conf_includes)
           cbits.target_cpkgs)
  in
  let cCamlIncludePath =
    fp (Analyze.get_ocaml_config_key "standard_library" bstate.bstate_config)
  in

  ( depIncludePaths,
    depIncludePathsD,
    depIncludePathsP,
    depLinkingPaths,
    cdepsIncludePaths,
    cCamlIncludePath )

(** Helper: Get the compile step for a module descriptor

    Determines whether a module needs interface compilation, module compilation, or directory
    packing based on its descriptor. *)
let get_compile_step_for_module modulesDeps stepsDag hier mdep =
  match mdep with
  | Module.DescFile f ->
      (* if it is a .mli only module ... *)
      if Filetype.of_filepath f.Module.File.path = Filetype.FileMLI then
        CompileInterface hier
      else begin
        if Module.has_interface mdep then
          Dag.add_edge (CompileModule hier) (CompileInterface hier) stepsDag;
        CompileModule hier
      end
  | Module.DescDir descdir ->
      let mStep = CompileDirectory hier in
      List.iter
        (fun dirChild ->
          let depChild = Hashtbl.find modulesDeps dirChild in
          let cStep =
            match depChild with
            | Module.DescFile f ->
                (* if it is a .mli only module ... *)
                if Filetype.of_filepath f.Module.File.path = Filetype.FileMLI then
                  CompileInterface dirChild
                else
                  CompileModule dirChild
            | Module.DescDir _ -> CompileDirectory dirChild
          in
          Dag.add_edge mStep cStep stepsDag)
        descdir.Module.Dir.modules;
      mStep

(** Helper: Build the module compilation steps DAG

    Processes module dependencies in topological order and builds the steps DAG. Uses a work queue
    to process modules with no dependencies first, then removes them and processes their dependents.
*)
let build_module_steps_dag modulesDeps target stepsDag =
  let h =
    hashtbl_map
      (fun dep ->
        match dep with
        | Module.DescDir _ -> []
        | Module.DescFile dfile -> dfile.Module.File.dep_cwd_modules)
      modulesDeps
  in
  while Hashtbl.length h > 0 do
    let freeModules = Hashtbl.fold (fun k v acc -> if v = [] then k :: acc else acc) h [] in
    if freeModules = [] then
      raise (Module.DependenciesProblem (hashtbl_keys h))
    else
      ();
    List.iter
      (fun m ->
        let mdep = Hashtbl.find modulesDeps m in
        let mStep = get_compile_step_for_module modulesDeps stepsDag m mdep in
        Dag.add_node mStep stepsDag;

        Hashtbl.iter
          (fun k v ->
            if k <> m then
              if List.mem m v then
                let kdep = Hashtbl.find modulesDeps k in
                match kdep with
                | Module.DescFile _ ->
                    if Module.has_interface kdep then
                      Dag.add_edges_connected
                        [ CompileModule k; CompileInterface k; mStep ]
                        stepsDag
                    else
                      Dag.add_edge (CompileModule k) mStep stepsDag
                | Module.DescDir _ -> Dag.add_edge (CompileDirectory k) mStep stepsDag)
          h)
      freeModules;
    let roots = Dag.get_roots stepsDag in
    List.iter
      (fun r ->
        match r with
        | CompileModule _ | CompileDirectory _ | CompileC _ ->
            Dag.add_edge (LinkTarget target) r stepsDag;
            Dag.add_edge (CheckTarget target) (LinkTarget target) stepsDag
        | _ -> ())
      roots;

    hashtbl_modify_all (fun v -> List.filter (fun x -> not (List.mem x freeModules)) v) h;
    List.iter (Hashtbl.remove h) freeModules
  done

(** Helper: Add C compilation tasks to the DAGs

    Processes C source files, determines their header dependencies, and adds compilation tasks to
    both the steps DAG and files DAG. *)
let add_c_compilation_tasks cbits buildDir stepsDag filesDag =
  if cbits.target_csources <> [] then
    let objDeps = run_ccdep cbits.target_cdir cbits.target_csources in

    List.iter
      (fun cSource ->
        let (fps : filepath list) =
          try List.assoc (Filetype.replace_extension cSource Filetype.FileO) objDeps
          with _ -> failwith ("cannot find dependencies for " ^ fn_to_string cSource)
        in
        let cFile = cbits.target_cdir </> cSource in
        let hFiles =
          List.map
            (fun x -> Filetype.make_id (Filetype.FileH, x))
            (List.filter (fun x -> Filetype.of_filepath x = Filetype.FileH) fps)
        in
        let oFile = buildDir </> (cSource <.> "o") in
        let cNode = Filetype.make_id (Filetype.FileC, cFile) in
        let oNode = Filetype.make_id (Filetype.FileO, oFile) in

        (* add C source information into the files DAG *)
        Dag.add_edge oNode cNode filesDag;
        Dag.add_children_edges oNode hFiles filesDag;

        (* add C source compilation task into the step DAG *)
        Dag.add_node (CompileC cSource) stepsDag)
      cbits.target_csources

(** Helper: Add cstubs generation tasks to the DAG

    If the target has cstubs configuration, adds the generation tasks with proper ordering: 1.
    GenerateCstubsTypes - generates types_generated.ml (runs first) 2. GenerateCstubsFunctions -
    generates C.ml and stubs.c (after bindings compile) 3. CompileCstubsC - compiles generated
    stubs.c 4. All must complete before LinkTarget *)
let add_cstubs_tasks target stepsDag =
  match target.target_cstubs with
  | None -> ()
  | Some cstubs ->
      (* Get the library name from target *)
      let libname =
        match target.target_name with
        | Target.Name.Lib l -> l
        | _ -> failwith "cstubs can only be used with libraries"
      in

      (* Add cstubs tasks to DAG *)
      let types_task = GenerateCstubsTypes libname in
      let funcs_task = GenerateCstubsFunctions libname in
      let compile_task = CompileCstubsC libname in

      Dag.add_node types_task stepsDag;
      Dag.add_node funcs_task stepsDag;
      Dag.add_node compile_task stepsDag;

      (* Ordering: types -> funcs -> compile_c *)
      Dag.add_edge funcs_task types_task stepsDag;
      Dag.add_edge compile_task funcs_task stepsDag;

      (* The generated_types module depends on GenerateCstubsTypes *)
      let generated_types_hier = Hier.of_string cstubs.cstubs_generated_types in
      (try
         let _ = Dag.get_node stepsDag (CompileModule generated_types_hier) in
         Dag.add_edge (CompileModule generated_types_hier) types_task stepsDag
       with Dag.DagNode_Not_found -> ());

      (* GenerateCstubsFunctions needs the compiled types_generated.cmo for stubgen *)
      (try
         let _ = Dag.get_node stepsDag (CompileModule generated_types_hier) in
         Dag.add_edge funcs_task (CompileModule generated_types_hier) stepsDag
       with Dag.DagNode_Not_found -> ());

      (* The entry point module depends on GenerateCstubsFunctions *)
      let entry_point_hier = Hier.of_string cstubs.cstubs_generated_entry_point in
      (try
         let _ = Dag.get_node stepsDag (CompileModule entry_point_hier) in
         Dag.add_edge (CompileModule entry_point_hier) funcs_task stepsDag
       with Dag.DagNode_Not_found -> ());

      (* The generated FOREIGN implementation module also depends on GenerateCstubsFunctions *)
      let generated_foreign_name = cstubs.cstubs_external_library_name ^ "_generated" in
      let generated_foreign_hier =
        Hier.of_string (Compat.string_capitalize generated_foreign_name)
      in
      (try
         let _ = Dag.get_node stepsDag (CompileModule generated_foreign_hier) in
         Dag.add_edge (CompileModule generated_foreign_hier) funcs_task stepsDag
       with Dag.DagNode_Not_found -> ());

      (* Add inter-module dependencies between cstubs-generated modules for correct link order:
         entry_point (C) -> generated_foreign (Otreesitter_stubs_generated) -> generated_types (Types_generated) *)
      (try
         let _ = Dag.get_node stepsDag (CompileModule entry_point_hier) in
         let _ = Dag.get_node stepsDag (CompileModule generated_foreign_hier) in
         Dag.add_edge (CompileModule entry_point_hier) (CompileModule generated_foreign_hier) stepsDag
       with Dag.DagNode_Not_found -> ());
      (try
         let _ = Dag.get_node stepsDag (CompileModule generated_foreign_hier) in
         let _ = Dag.get_node stepsDag (CompileModule generated_types_hier) in
         Dag.add_edge (CompileModule generated_foreign_hier) (CompileModule generated_types_hier) stepsDag
       with Dag.DagNode_Not_found -> ());

      (* Helper: extract the top-level module from a functor path like "Bindings.Types" -> "Bindings" *)
      let get_module_from_functor_path hier = Hier.of_modname (Hier.root hier) in

      (* If there's a type description functor, both types_task and funcs_task depend on its module.
       types_task needs it to use Cstubs_structs.write_c with the Types functor for struct discovery.
       funcs_task needs it to use Cstubs.write_c/write_ml with the Functions functor. *)
      (match cstubs.cstubs_type_description with
      | Some desc -> (
          let bindings_module = get_module_from_functor_path desc.cstubs_functor in
          try
            let _ = Dag.get_node stepsDag (CompileModule bindings_module) in
            (* types_task depends on Bindings for Cstubs_structs.write_c *)
            Dag.add_edge types_task (CompileModule bindings_module) stepsDag;
            (* funcs_task also depends on Bindings for Cstubs.write_c *)
            Dag.add_edge funcs_task (CompileModule bindings_module) stepsDag
          with Dag.DagNode_Not_found -> ())
      | None -> ());

      (* If there's a function description functor, funcs_task depends on its module *)
      (match cstubs.cstubs_function_description with
      | Some desc -> (
          let bindings_module = get_module_from_functor_path desc.cstubs_functor in
          try
            let _ = Dag.get_node stepsDag (CompileModule bindings_module) in
            Dag.add_edge funcs_task (CompileModule bindings_module) stepsDag
          with Dag.DagNode_Not_found -> ())
      | None -> ());

      (* Link depends on CompileCstubsC *)
      Dag.add_edge (LinkTarget target) compile_task stepsDag;
      Dag.add_edge (CheckTarget target) (LinkTarget target) stepsDag

(** Add generate block tasks to the DAG *)
let add_generate_block_tasks target stepsDag =
  List.iter (fun (gen_block : Target.target_generate) ->
    let task = RunGenerateBlock gen_block in
    Dag.add_node task stepsDag;

    (* The generated module depends on the generate block running first *)
    let output_hier = gen_block.generate_module in
    (try
       let _ = Dag.get_node stepsDag (CompileModule output_hier) in
       Dag.add_edge (CompileModule output_hier) task stepsDag
     with Dag.DagNode_Not_found -> ());

    (* Link depends on all generate blocks completing *)
    Dag.add_edge (LinkTarget target) task stepsDag
  ) target.Target.target_generates

(* get every module description
 * and their relationship with each other
 *)
let get_modules_desc bstate target toplevelModules =
  let autogenDir = Dist.get_build_exn Dist.Autogen in
  let modulesDeps = Hashtbl.create 64 in
  let file_search_paths hier =
    List.map (fun dir -> dir <//> Hier.to_dirpath hier) target.target_obits.target_srcdir
    @ [ autogenDir ]
  in

  (* Check if a module is cstubs-generated (will be created during build) *)
  let is_cstubs_generated_module hier =
    match target.target_cstubs with
    | Some cstubs ->
        let module_name = Hier.to_string hier in
        (* All three modules are generated from cstubs config:
         - <lib>_generated: FOREIGN implementation
         - generated-types: type bindings
         - generated-entry-point: entry module *)
        let foreign_name =
          Compat.string_capitalize (cstubs.Target.cstubs_external_library_name ^ "_generated")
        in
        let types_name = Compat.string_capitalize cstubs.Target.cstubs_generated_types in
        let entry_name = Compat.string_capitalize cstubs.Target.cstubs_generated_entry_point in
        module_name = foreign_name || module_name = types_name || module_name = entry_name
    | None -> false
  in

  (* Check if a module is from a generate block (will be created during build) *)
  let find_generate_block_for_module hier =
    let module_name = Hier.to_string hier in
    try
      Some (List.find (fun (gen : Target.target_generate) ->
        Hier.to_string gen.generate_module = module_name
      ) target.Target.target_generates)
    with Not_found -> None
  in

  let targetPP = Ppx_resolver.get_target_pp bstate target target.target_obits.target_pp in

  let get_one hier =
    let moduleName = Hier.to_string hier in
    verbose Verbose "Analysing %s\n%!" moduleName;
    (* For cstubs-generated modules, return a minimal description without file analysis *)
    if is_cstubs_generated_module hier then (
      (* Get library-specific autogen dir for cstubs generated files *)
      let cstubs_autogen_dir =
        match target.target_name with
        | Target.Name.Lib libname -> autogenDir </> fn (Libname.to_string libname)
        | _ -> autogenDir
      in
      let ml_filename = fn (Compat.string_uncapitalize moduleName ^ ".ml") in
      let target_path = cstubs_autogen_dir </> ml_filename in
      verbose Verbose "  %s is cstubs-generated, using synthetic description at %s\n%!" moduleName
        (fp_to_string target_path);
      (* Register the synthetic entry in Hier so get_dest_file can find it *)
      Hier.register_synthetic_entry hier cstubs_autogen_dir target_path;
      (* Return a minimal module description - the file will be created during build *)
      Module.make_file NoThread target_path 0.0 SimpleModule None Pp.none [] [] [])
    (* For generate-block modules, return a minimal description *)
    else if find_generate_block_for_module hier <> None then (
      let ml_filename = fn (Compat.string_uncapitalize moduleName ^ ".ml") in
      let target_path = autogenDir </> ml_filename in
      verbose Verbose "  %s is from generate block, using synthetic description at %s\n%!" moduleName
        (fp_to_string target_path);
      (* Register the synthetic entry in Hier so get_dest_file can find it *)
      Hier.register_synthetic_entry hier autogenDir target_path;
      (* Return a minimal module description - the file will be created during build *)
      Module.make_file NoThread target_path 0.0 SimpleModule None Pp.none [] [] [])
    else
      let file_entry =
        let paths = file_search_paths hier in
        try Hier.get_file_entry hier paths with Not_found -> raise (Module.NotFound (paths, hier))
      in
      let _srcPath, srcDir =
        match file_entry with
        | Hier.FileEntry (s, d) | Hier.DirectoryEntry (s, d) | Hier.GeneratedFileEntry (s, d, _) ->
            (s, d)
      in
      let module_desc_ty =
        if Filesystem.is_dir srcDir then
          discover_directory_modules srcDir hier
        else
          let _srcPath, srcFile, intfFile =
            match file_entry with
            | Hier.FileEntry (path, file) -> (path, file, Hier.ml_to_ext file Filetype.FileMLI)
            | Hier.DirectoryEntry (path, file) -> (path, file, Hier.ml_to_ext file Filetype.FileMLI)
            | Hier.GeneratedFileEntry (_path, file, generated) ->
                let src_file = path_basename file in
                let actual_src_path = Dist.get_build_exn (Dist.Target target.target_name) in
                let full_dest_file = actual_src_path </> generated in
                let intf_file = Hier.ml_to_ext full_dest_file Filetype.FileMLI in
                if
                  (not (Filesystem.exists full_dest_file))
                  || Filesystem.get_modification_time full_dest_file
                     < Filesystem.get_modification_time file
                then
                  Generators.run (actual_src_path </> chop_extension src_file) file moduleName;
                (actual_src_path, full_dest_file, intf_file)
          in
          let modTime = Filesystem.get_modification_time srcFile in
          let hasInterface = Filesystem.exists intfFile in
          let intfModTime = Filesystem.get_modification_time intfFile in

          (* augment pp if needed with per-file dependencies *)
          let per_settings = find_extra_matching target (Hier.to_string hier) in
          let per_pp =
            let l = List.filter (fun x -> x.target_extra_pp <> None) per_settings in
            if List.length l > 0 then
              (List.hd l).target_extra_pp
            else
              None
          in
          let pp =
            match (target.target_obits.target_pp, per_pp) with
            | None, None -> Pp.none
            | None, Some preprocessor | Some _, Some preprocessor ->
                let perPP = Ppx_resolver.get_target_pp bstate target per_pp in
                let extraDeps =
                  List.concat (List.map (fun x -> x.target_extra_builddeps) per_settings)
                in
                Pp.append perPP
                  (Ppx_resolver.get_syntax_pp bstate preprocessor (List.map fst extraDeps))
            | Some preprocessor, None ->
                (* FIXME: we should re-use the dependency DAG here, otherwise we might end up in the case
                 * where the extra dependencies are depending not in the correct order
                 *)
                let extraDeps =
                  List.concat (List.map (fun x -> x.target_extra_builddeps) per_settings)
                in
                Pp.append targetPP
                  (Ppx_resolver.get_syntax_pp bstate preprocessor (List.map fst extraDeps))
          in

          (* Resolve PPX flags for this module *)
          let ppx = resolve_module_ppx_flags bstate target in

          verbose Debug "  %s has mtime %f\n%!" moduleName modTime;
          if hasInterface then
            verbose Debug "  %s has interface (mtime=%f)\n%!" moduleName intfModTime;

          (* Analyze module dependencies *)
          let cwdDeps, otherDeps, use_thread =
            analyze_module_dependencies srcFile hier pp file_search_paths
          in

          let intfDesc =
            if hasInterface then
              Some (Module.Intf.make intfModTime intfFile)
            else
              None
          in
          Module.make_file use_thread srcFile modTime
            (match file_entry with
            | Hier.FileEntry _ -> SimpleModule
            | Hier.GeneratedFileEntry _ -> GeneratedModule
            | Hier.DirectoryEntry _ -> failwith "unexpected DirectoryEntry in get_modules_desc")
            intfDesc pp
            ((target.target_obits.target_oflags
             @ List.concat
                 (List.map
                    (fun x -> x.target_extra_oflags)
                    (find_extra_matching target (Hier.to_string hier))))
            @ ppx)
            cwdDeps otherDeps
      in
      module_desc_ty
  in
  let rec loop modname =
    if Hashtbl.mem modulesDeps modname then
      ()
    else
      let mdesc = get_one modname in
      Hashtbl.add modulesDeps modname mdesc;
      (* TODO: don't query single modules at time, where ocamldep supports M modules.
         tricky with single file syntax's pragma. *)
      match mdesc with
      | Module.DescFile dfile -> List.iter loop dfile.Module.File.dep_cwd_modules
      | Module.DescDir ddir -> List.iter loop ddir.Module.Dir.modules
  in
  List.iter (fun m -> loop m) toplevelModules;
  modulesDeps

(* prepare modules dependencies and various compilation state
 * that is going to be required for compilation and linking.
 *)
let prepare_target_ bstate buildDir target toplevelModules =
  let autogenDir = Dist.get_build_exn Dist.Autogen in
  let buildDirP = buildDir </> fn "opt-p" in
  let buildDirD = buildDir </> fn "opt-d" in

  let cbits = target.target_cbits in
  let obits = target.target_obits in

  verbose Verbose "preparing compilation for %s\n%!" (Target.get_target_name target);

  let modulesDeps = get_modules_desc bstate target toplevelModules in

  (* create 2 dags per target
   * - stepsDag is a DAG of all the tasks to achieve the target (compilation only, not linking yet)
   * - filesDag is a DAG of all the files dependencies (C files & H files)
   *)
  let get_dags () =
    let filesDag = Dag.init () in
    let stepsDag = Dag.init () in

    (* Build the module dependency DAG *)
    build_module_steps_dag modulesDeps target stepsDag;

    (* Add C compilation tasks *)
    add_c_compilation_tasks cbits buildDir stepsDag filesDag;

    (* Add cstubs generation tasks if configured *)
    add_cstubs_tasks target stepsDag;

    (* Add generate block tasks *)
    add_generate_block_tasks target stepsDag;

    (stepsDag, filesDag)
  in
  let dag, fdag = get_dags () in

  if gconf.dump_dot then (
    let dotDir = Dist.create_build Dist.Dot in
    let path = dotDir </> fn (Target.get_target_name target ^ ".dot") in
    let reducedDag = Dag.transitive_reduction dag in
    let dotContent =
      Dag.to_dot string_of_compile_step (Target.get_target_name target) true reducedDag
    in
    Filesystem.write_file path dotContent;

    let path = dotDir </> fn (Target.get_target_name target ^ ".files.dot") in
    let dotContent =
      Dag.to_dot
        (fun fdep ->
          Filetype.to_string (Filetype.get_type fdep) ^ " " ^ fp_to_string (Filetype.get_path fdep))
        (Target.get_target_name target) true fdag
    in
    Filesystem.write_file path dotContent);

  (* Compute all dependency paths for compilation and linking *)
  let ( depIncludePaths,
        depIncludePathsD,
        depIncludePathsP,
        depLinkingPaths,
        cdepsIncludePaths,
        cCamlIncludePath ) =
    compute_dependency_paths bstate target
  in

  {
    compilation_modules = modulesDeps;
    compilation_csources = cbits.target_csources;
    compilation_dag = dag;
    compilation_pp = Pp.none;
    compilation_filesdag = fdag;
    compilation_builddir_c = buildDir;
    compilation_builddir_ml =
      (fun m ->
        match m with
        | Normal -> buildDir
        | WithDebug -> buildDirD
        | WithProf -> buildDirP)
      (* Add library-specific autogen dir for cstubs-generated modules *);
    compilation_include_paths =
      (fun m hier ->
        let cstubs_autogen_dir =
          match (target.target_cstubs, target.target_name) with
          | Some _, Target.Name.Lib libname -> [ autogenDir </> fn (Libname.to_string libname) ]
          | _ -> []
        in
        ((match m with
           | Normal -> buildDir
           | WithDebug -> buildDirD
           | WithProf -> buildDirP)
        <//> Hier.to_dirpath hier)
        :: cstubs_autogen_dir
        @ [ autogenDir ]
        @ List.map (fun dir -> dir <//> Hier.to_dirpath hier) obits.target_srcdir
        @
        match m with
        | Normal -> depIncludePaths
        | WithDebug -> depIncludePathsD
        | WithProf -> depIncludePathsP);
    compilation_linking_paths = [ buildDir ] @ depLinkingPaths;
    compilation_linking_paths_p = [ buildDirP; buildDir ] @ depLinkingPaths;
    compilation_linking_paths_d = [ buildDirD; buildDir ] @ depLinkingPaths;
    compilation_c_include_paths =
      [ cbits.target_cdir ] @ cdepsIncludePaths @ [ cCamlIncludePath; autogenDir ];
    compilation_c_linking_paths = [ buildDir ];
  }

let prepare_target bstate buildDir target toplevelModules =
  try prepare_target_ bstate buildDir target toplevelModules
  with exn ->
    verbose Verbose "Prepare.target : uncaught exception %s\n%!" (Printexc.to_string exn);
    raise exn
