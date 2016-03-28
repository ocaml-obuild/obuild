(*
 * gather dependencies in hashtable and DAGs
 * and create compilation state for one target
 *)
open Ext.Fugue
open Ext.Filepath
open Ext
open Analyze
open Types
open Helper
open Gconf
open Target
open Dependencies

type use_thread_flag = NoThread | WithThread
type thread_type = VMThread | PosixThread | DefaultThread | NoThreads
type ocaml_file_type = GeneratedModule | SimpleModule

module Module = struct
  exception DependsItself of Hier.t
  exception DependenciesProblem of Hier.t list
  exception DependencyNoOutput
  exception NotFound of (filepath list * Hier.t)

  module Intf = struct
    type t = {
      mtime : float;
      path : filepath
    }
    let make mtime path = { mtime; path }
  end

  module File = struct
    type t = {
      use_threads  : use_thread_flag;
      path    : filepath;
      mtime   : float;
      type_   : ocaml_file_type;
      intf_desc   : Intf.t option;
      use_pp      : Pp.t;
      oflags      : string list;
      dep_cwd_modules    : Hier.t list;
      dep_other_modules  : Modname.t list;
    }
    let make use_threads path mtime type_ intf_desc use_pp oflags dep_cwd_modules dep_other_modules =
      { use_threads; path; mtime; type_; intf_desc; use_pp; oflags; dep_cwd_modules; dep_other_modules }
  end

  module Dir = struct
    type t = {
      path    : filepath;
      modules : Hier.t list
    }
    let make path modules = {path; modules}
  end

  type t = DescFile of File.t | DescDir of Dir.t

  let file_has_interface mdescfile =
    maybe false (fun _ -> true) mdescfile.File.intf_desc

  let has_interface = function
    | DescFile dfile -> file_has_interface dfile
    | DescDir _      -> false

  let make_dir path modules = DescDir (Dir.make path modules)
  let make_file use_threads path mtime type_ intf_desc use_pp oflags dep_cwd_modules dep_other_modules =
    DescFile 
      (File.make use_threads path mtime type_ intf_desc use_pp oflags dep_cwd_modules dep_other_modules)
end

 (* live for the whole duration of a building process
 * which include compilations and linkings
*)
type build_state = { bstate_config : project_config }

type dir_spec = {
  src_dir      : filepath;
  dst_dir      : filepath;
  include_dirs : filepath list;
}

type compile_step = CompileModule    of Hier.t
                  | CompileInterface of Hier.t
                  | CompileDirectory of Hier.t
                  | CompileC         of filename
                  | LinkTarget       of target
                  | CheckTarget      of target

let string_of_compile_step cs = match cs with
  | CompileDirectory x -> "dir " ^ (Hier.to_string x)
  | CompileModule x    -> "mod " ^ (Hier.to_string x)
  | CompileInterface x -> "intf " ^ (Hier.to_string x)
  | CompileC x         -> "C " ^ (fn_to_string x)
  | LinkTarget x       -> "link " ^ (Target.get_target_name x)
  | CheckTarget x      -> "check " ^ (Target.get_target_name x)

(* represent a single compilation *)
type compilation_state = {
  compilation_modules  : (Hier.t, Module.t) Hashtbl.t;
  compilation_csources : filename list;
  compilation_dag      : compile_step Dag.t;
  compilation_pp       : Pp.t;
  compilation_filesdag : Filetype.id Dag.t;
  compilation_builddir_c  : filepath;
  compilation_builddir_ml : Types.ocaml_compilation_option -> filepath;
  compilation_include_paths : Types.ocaml_compilation_option -> Hier.t -> filepath list;
  compilation_linking_paths : filepath list;
  compilation_linking_paths_d : filepath list;
  compilation_linking_paths_p : filepath list;
  compilation_c_include_paths : filepath list;
  compilation_c_linking_paths : filepath list;
}

let init project = { bstate_config = project }

let get_compilation_order cstate =
  let filter_modules t : Hier.t option = match t with
    | (CompileC _) | (CompileInterface _) | (LinkTarget _) | (CheckTarget _) -> None
    | (CompileDirectory m) | (CompileModule m) -> if Hier.lvl m = 0 then Some m else None
  in
  list_filter_map filter_modules (Dagutils.linearize cstate.compilation_dag)

let camlp4Libname = Libname.of_string "camlp4"
let syntaxPredsCommon = [Meta.Predicate.Syntax;Meta.Predicate.Preprocessor]

let get_p4pred = function
  | Pp.Type.CamlP4O -> Meta.Predicate.Camlp4o
  | Pp.Type.CamlP4R -> Meta.Predicate.Camlp4r

let get_syntax_pp bstate preprocessor buildDeps =
  let conf = bstate.bstate_config in
  let p4pred = get_p4pred preprocessor in
  let stdlib = fp (get_ocaml_config_key "standard_library" conf) in
  list_filter_map (fun spkg ->
      if Analyze.is_pkg_internal conf spkg
      then (
        let lib = Project.find_lib bstate.bstate_config.project_file spkg in
        if lib.Project.Library.syntax
        then (
          (* TODO need to make sure that the bytecode option has been enabled for the syntax library *)
          let dir = Dist.get_build_exn (Dist.Target (Name.Lib lib.Project.Library.name)) in
          Some [fp_to_string (dir </> Libname.to_cmca ByteCode Normal lib.Project.Library.name) ]
        ) else None
      ) else (
        let meta = Metacache.get_from_cache spkg in
        let preds =
          if spkg = camlp4Libname
          then p4pred :: syntaxPredsCommon
          else syntaxPredsCommon
        in
        if Meta.Pkg.is_syntax meta spkg
        then (
          let includePath = Meta.getIncludeDir stdlib meta in
          Some ["-I"; fp_to_string includePath; Meta.Pkg.get_archive meta spkg preds]
        ) else
          None
      )
    ) buildDeps

let get_target_pp bstate target = function
  | None    -> Pp.none
  | Some pp ->
    let conf = bstate.bstate_config in
    let nodes = List.rev (Taskdep.linearize conf.project_pkgdeps_dag Taskdep.FromParent
                            [Analyze.Target target.target_name]) in
    let syntaxPkgs = list_filter_map (fun node ->
        match node with
        | Dependency dep -> Some dep
        | _              -> None
      ) nodes
    in
    verbose Verbose " all packages : [%s]\n%!" (Utils.showList "," Libname.to_string syntaxPkgs);
    let p4pred = get_p4pred pp in
    let p4Meta = Metacache.get_from_cache camlp4Libname in
    let preproc = (snd p4Meta).Meta.Pkg.preprocessor in
    let archive = [Meta.Pkg.get_archive p4Meta camlp4Libname (p4pred::syntaxPredsCommon)] in
    (*verbose Verbose " camlp4 strs: [%s]\n%!" (Utils.showList "] [" id camlp4Strs);*)
    let camlp4Strs = get_syntax_pp bstate pp syntaxPkgs in
    Pp.some preproc (archive :: camlp4Strs)


(* get every module description
 * and their relationship with each other
*)
let get_modules_desc bstate target toplevelModules =
  let autogenDir = Dist.get_build_exn Dist.Autogen in
  let modulesDeps = Hashtbl.create 64 in
  let file_search_paths hier = (List.map (fun dir -> dir <//> Hier.to_dirpath hier) target.target_obits.target_srcdir) @ [autogenDir] in

  let targetPP = get_target_pp bstate target target.target_obits.target_pp in

  let get_one hier =
    let moduleName = Hier.to_string hier in
    verbose Verbose "Analysing %s\n%!" moduleName;
    let file_entry =
      let paths = (file_search_paths hier) in
      try Hier.get_file_entry hier paths
      with Not_found ->
        raise (Module.NotFound (paths, hier))
    in
    let (srcPath,srcDir) =
      match file_entry with
      | Hier.FileEntry (s, d) | Hier.DirectoryEntry (s, d) | Hier.GeneratedFileEntry (s, d, _) -> (s, d)
    in
    let module_desc_ty =
      if Filesystem.is_dir srcDir
      then (
        let modules = Filesystem.list_dir_pred_map (fun f ->
            let fp = srcDir </> f in
            if Filesystem.is_dir fp
            then
              (* Should avoid directory such as .git/.svn etc. *)
              if not (Modname.string_all Modname.char_is_valid_modchar (fn_to_string f)) then
                None
              else
                Some (Modname.of_directory f)
            else (match Filetype.of_filepath fp with
                | Filetype.FileML  -> Some (Modname.of_filename f)
                | Filetype.FileMLI  ->
                  if (Filesystem.exists (srcDir </> ((chop_extension f) <.> "ml"))) then
                    None
                  else (* lonely mli *)
                    Some (Modname.of_filename f)
                | Filetype.FileOther s -> if Generators.is_generator_ext s then Some (Modname.of_filename f)
                  else None
                | _                -> None
              )
          ) srcDir
        in
        Module.make_dir currentDir (List.map (fun m -> Hier.append hier m) modules)
      ) else (
        let (srcPath, srcFile, intfFile) =
          match file_entry with
          | Hier.FileEntry (path, file) ->
            (path, file, (Hier.ml_to_ext file Filetype.FileMLI))
          | Hier.DirectoryEntry (path, file) ->
            (path, file, (Hier.ml_to_ext file Filetype.FileMLI))
          | Hier.GeneratedFileEntry (path, file, generated) ->
            let src_file = path_basename file in
            let actual_src_path = Dist.get_build_exn (Dist.Target target.target_name) in
            let full_dest_file = actual_src_path </> generated in
            let intf_file = Hier.ml_to_ext full_dest_file Filetype.FileMLI in
            if not (Filesystem.exists full_dest_file) ||
               ((Filesystem.getModificationTime full_dest_file) < (Filesystem.getModificationTime file))
            then
              Generators.run (actual_src_path </> (chop_extension src_file)) file moduleName;
            (actual_src_path, full_dest_file, intf_file)
        in
        let modTime = Filesystem.getModificationTime srcFile in
        let hasInterface = Filesystem.exists intfFile in
        let intfModTime = Filesystem.getModificationTime intfFile in

        (* augment pp if needed with per-file dependencies *)
        let per_settings = find_extra_matching target (Hier.to_string hier) in
        let per_pp =
          let l = List.filter (fun x -> x.target_extra_pp <> None) per_settings in
          if(List.length l) > 0 then
            (List.hd l).target_extra_pp
          else
            None
        in
        let pp = match target.target_obits.target_pp,per_pp with
          | None,None              -> Pp.none
          | None,Some preprocessor | Some _, Some preprocessor ->
            let perPP = get_target_pp bstate target per_pp in
            let extraDeps = List.concat (List.map (fun x -> x.target_extra_builddeps) per_settings) in
            Pp.append perPP (get_syntax_pp bstate preprocessor (List.map fst extraDeps))
          | Some preprocessor,None ->
            (* FIXME: we should re-use the dependency DAG here, otherwise we might end up in the case
             * where the extra dependencies are depending not in the correct order
            *)
            let extraDeps = List.concat (List.map (fun x -> x.target_extra_builddeps) per_settings) in
            Pp.append targetPP (get_syntax_pp bstate preprocessor (List.map fst extraDeps))
        in

        let full_path include_path name =
          match name.[0] with
          | '.' -> (fp_to_string include_path) ^ "/" ^ name
          | _ -> name
        in
        let stdlib = fp (get_ocaml_config_key "standard_library" bstate.bstate_config) in
        let get_ppx_ppxopt fpath meta libname = 
          let includePath = Meta.getIncludeDir stdlib (fpath,meta) in
          let pkg = Meta.Pkg.find libname.Libname.subnames meta in
          let ppx = pkg.Meta.Pkg.ppx in
          let ppxopt = pkg.Meta.Pkg.ppxopt in
          (includePath, ppx, ppxopt)
        in
        let ppx =
          let target_deps = get_all_builddeps target in
          let dag = bstate.bstate_config.project_pkgdeps_dag in
          let deps_lists = list_filter_map (fun (l,_) ->
              let dag_dep = Analyze.Dependency l in
              if (Dag.existsNode dag_dep dag) then begin
                let children = Dag.getChildren_full dag dag_dep in
                let deps = list_filter_map (fun d -> match d with Analyze.Target _ -> None
                                                                | Analyze.Dependency l -> Some l)
                    children in
                let uniq_deps = list_uniq deps in
                Some (l :: uniq_deps)
              end else
                None
            ) target_deps in
          let ppx_list = List.map (fun l ->
              let (ppxs,ppxopts) = List.fold_left (fun (ppxs,ppxopts) d ->
                  match (Metacache.find d.Libname.main_name) with
                  | None -> (ppxs,ppxopts)
                  | Some (fpath, meta) ->
                    let (includePath, ppx, ppxopt) = get_ppx_ppxopt fpath meta d in
                    let ppxs_ = match ppx with None -> ppxs
                                             | Some (_,s) -> (includePath,s,d) :: ppxs in
                    let ppxopts_ = ppxopts @ (List.map (fun (_,s) ->
                        let ppxargs = string_split ',' s in
                        (includePath, ppxargs)
                      ) ppxopt) in
                    (ppxs_, ppxopts_)
                ) ([],[]) (List.rev l) in
              let ppxs = list_uniq ppxs in
              if (List.length ppxs) > 1 then
                failwith ("More than 1 ppx " ^ (String.concat ", " (List.map (fun (_,s,_) -> s) ppxs)));
              if (List.length ppxs) = 0 then
                []
              else
                let (includePath,ppx_name,ppx_lib) = List.hd ppxs in
                List.iter (fun (_,ss) ->
                    let res = (Libname.of_string (List.hd ss)) = ppx_lib in
                    if not res then
                      failwith ("Different ppx " ^ ppx_name ^ " <> " ^ (List.hd ss))
                  ) ppxopts;
                (full_path includePath ppx_name) :: (List.map (fun (includePath,args) ->
                    String.concat " " (List.map (fun a -> full_path includePath a) (List.tl args))) ppxopts)
            ) deps_lists in
          let ppx_list = no_empty [] ppx_list in
          List.flatten (List.map (fun l -> ["-ppx"; String.concat " " l]) ppx_list)
        in

        verbose Debug "  %s has mtime %f\n%!" moduleName modTime;
        if hasInterface then
          verbose Debug "  %s has interface (mtime=%f)\n%!" moduleName intfModTime;

        let dopt = {
          dep_includes = file_search_paths hier;
          dep_pp       = pp
        } in
        let allDeps = match runOcamldep dopt srcFile with
          | []   -> raise Module.DependencyNoOutput
          | ml::mli::_ -> list_uniq (ml @ mli)
          | x::_ -> x
        in
        verbose Debug "  %s depends on %s\n%!" moduleName (String.concat "," allDeps);
        let (cwdDepsInDir, otherDeps) = List.partition (fun dep ->
            try
              let entry = Hier.get_file_entry (Hier.of_modname dep) (file_search_paths hier) in
              match entry with
              | Hier.DirectoryEntry (p,_) | Hier.FileEntry (p,_) | Hier.GeneratedFileEntry (p,_,_) ->
                List.mem p (file_search_paths hier)
            with
              Not_found -> false
          ) allDeps in
        verbose Debug "  %s internally depends on %s\n%!" moduleName (String.concat "," (List.map Modname.to_string cwdDepsInDir));
        let use_thread =
          if List.mem (Modname.wrap "Thread") otherDeps
          || List.mem (Modname.wrap "Condition") otherDeps
          || List.mem (Modname.wrap "Mutex") otherDeps
          then WithThread
          else NoThread
        in
        let cwdDeps = List.map (fun x -> maybe (Hier.make [x]) (fun z -> Hier.append z x) (Hier.parent hier)) cwdDepsInDir in
        (if List.mem hier cwdDeps then
           raise (Module.DependsItself hier)
        );
        let intfDesc =
          if hasInterface
          then Some (Module.Intf.make intfModTime intfFile)
          else None
        in 
        Module.make_file use_thread srcFile modTime 
          (match file_entry with Hier.FileEntry _ -> SimpleModule | Hier.GeneratedFileEntry _ -> GeneratedModule)
          intfDesc pp
          ((target.target_obits.target_oflags @
           (List.concat (List.map (fun x -> x.target_extra_oflags) (find_extra_matching target (Hier.to_string hier))))) @ ppx)
          cwdDeps otherDeps
      )
    in
    module_desc_ty

  in
  let rec loop modname =
    if Hashtbl.mem modulesDeps modname
    then ()
    else (
      let mdesc = get_one modname in
      Hashtbl.add modulesDeps modname mdesc;
      (* TODO: don't query single modules at time, where ocamldep supports M modules.
         tricky with single file syntax's pragma. *)
      match mdesc with
      | Module.DescFile dfile -> List.iter loop dfile.Module.File.dep_cwd_modules
      | Module.DescDir  ddir  -> List.iter loop ddir.Module.Dir.modules
    )
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
   **)
  let get_dags () =
    let filesDag = Dag.init () in
    let stepsDag = Dag.init () in
    let h = hashtbl_map (fun dep -> match dep with
        | Module.DescDir _      -> []
        | Module.DescFile dfile -> dfile.Module.File.dep_cwd_modules
      ) modulesDeps
    in
    while Hashtbl.length h > 0 do
      let freeModules = Hashtbl.fold (fun k v acc -> if v = [] then k :: acc else acc) h [] in
      if freeModules = []
      then raise (Module.DependenciesProblem (hashtbl_keys h))
      else ();
      List.iter (fun m ->
          let mdep = Hashtbl.find modulesDeps m in
          let mStep = match mdep with
            | Module.DescFile f ->
              (* if it is a .mli only module ... *)
              if  (Filetype.of_filepath f.Module.File.path) = Filetype.FileMLI then
                CompileInterface m
              else begin
                if Module.has_interface mdep then (
                  Dag.addEdge (CompileModule m) (CompileInterface m) stepsDag;
                );
                CompileModule m
              end
            | Module.DescDir descdir ->
              let mStep = CompileDirectory m in
              List.iter (fun dirChild ->
                  (*printf "  %s depends %s" (string_of_compilation_step mStep) ((Compi *)
                  let depChild = Hashtbl.find modulesDeps dirChild in
                  let cStep = match depChild with
                    | Module.DescFile f ->
                      (* if it is a .mli only module ... *)
                      if  (Filetype.of_filepath f.Module.File.path) = Filetype.FileMLI then
                        CompileInterface dirChild
                      else
                        CompileModule dirChild
                    | Module.DescDir _ -> CompileDirectory dirChild
                  in
                  Dag.addEdge mStep cStep stepsDag
                ) descdir.Module.Dir.modules;
              mStep
          in
          Dag.addNode mStep stepsDag;

          Hashtbl.iter (fun k v ->
              if k <> m then (
                if List.mem m v then (
                  let kdep = Hashtbl.find modulesDeps k in
                  match kdep with
                  | Module.DescFile _ ->
                    if Module.has_interface kdep
                    then (
                      Dag.addEdgesConnected [CompileModule k; CompileInterface k; mStep] stepsDag
                    ) else
                      Dag.addEdge (CompileModule k) mStep stepsDag
                  | Module.DescDir _ ->
                    Dag.addEdge (CompileDirectory k) mStep stepsDag
                )
              )
            ) h;
        ) freeModules;
      let roots = Dag.getRoots stepsDag in
      List.iter (fun r ->
          match r with
          | CompileModule _ | CompileDirectory _->
            Dag.addEdge (LinkTarget target) r stepsDag;
            Dag.addEdge (CheckTarget target) (LinkTarget target) stepsDag;
          | _ -> ()
        )
        roots;

      hashtbl_modify_all (fun v -> List.filter (fun x -> not (List.mem x freeModules)) v) h;
      List.iter (Hashtbl.remove h) freeModules;
    done;

    (* just append each C sources as single node in the stepsDag *)
    if cbits.target_csources <> [] then (
      let objDeps = runCCdep cbits.target_cdir cbits.target_csources in

      List.iter (fun cSource ->
          let (fps : filepath list) =
            try List.assoc (Filetype.replace_extension cSource Filetype.FileO) objDeps
            with _ -> failwith ("cannot find dependencies for " ^ fn_to_string cSource)
          in
          let cFile = cbits.target_cdir </> cSource in
          let hFiles = List.map (fun x -> Filetype.make_id (Filetype.FileH, x))
              (List.filter (fun x -> Filetype.of_filepath x = Filetype.FileH) fps)
          in
          let oFile = buildDir </> (cSource <.> "o") in
          let cNode = Filetype.make_id (Filetype.FileC, cFile) in
          let oNode = Filetype.make_id (Filetype.FileO, oFile) in

          (* add C source information into the files DAG *)
          Dag.addEdge oNode cNode filesDag;
          Dag.addChildrenEdges oNode hFiles filesDag;

          (* add C source compilation task into the step DAG *)
          Dag.addNode (CompileC cSource) stepsDag
        ) cbits.target_csources;
    );

    (stepsDag, filesDag)
  in
  let (dag, fdag) = get_dags () in

  if gconf.dump_dot
  then (
    let dotDir = Dist.create_build Dist.Dot in
    let path = dotDir </> fn (Target.get_target_name target ^ ".dot") in
    let reducedDag = Dag.transitive_reduction dag in
    let dotContent = Dag.toDot string_of_compile_step (Target.get_target_name target) true reducedDag in
    Filesystem.writeFile path dotContent;

    let path = dotDir </> fn (Target.get_target_name target ^ ".files.dot") in
    let dotContent = Dag.toDot (fun fdep -> Filetype.to_string (Filetype.get_type fdep) ^ " " ^ 
                                            fp_to_string (Filetype.get_path fdep))
        (Target.get_target_name target) true fdag in
    Filesystem.writeFile path dotContent;

  );

  let conf = bstate.bstate_config in
  let stdlib = fp (get_ocaml_config_key "standard_library" conf) in

  let depPkgs = Analyze.get_pkg_deps target conf in
  let (depsInternal,depsSystem) = List.partition (fun dep ->
      match Hashtbl.find conf.project_dep_data dep with
      | Internal -> true
      | _ -> false) depPkgs in
  let depIncPathInter = List.map (fun dep ->
      Dist.get_build_exn (Dist.Target (Name.Lib dep))) depsInternal in
  let depIncPathSystem = List.map (fun dep ->
      Meta.getIncludeDir stdlib (Metacache.get_from_cache dep)) depsSystem in
  let depIncludePaths = depIncPathInter @ depIncPathSystem in
  let depIncludePathsD = List.map (fun fp -> fp </> fn "opt-d") depIncPathInter @ depIncPathSystem in
  let depIncludePathsP = List.map (fun fp -> fp </> fn "opt-p") depIncPathInter @ depIncPathSystem in
  let depLinkingPaths =
    List.map (fun dep ->
        match Hashtbl.find conf.project_dep_data dep with
        | Internal -> Dist.get_build_exn (Dist.Target (Name.Lib dep))
        | System   -> Meta.getIncludeDir stdlib (Metacache.get_from_cache dep)
      ) depPkgs
  in
  let cdepsIncludePaths : filepath list =
    cbits.target_clibpaths
    @ List.concat (List.map (fun (cpkg,_) -> (Hashtbl.find bstate.bstate_config.project_cpkgs cpkg).cpkg_conf_includes) cbits.target_cpkgs)
  in
  let cCamlIncludePath = fp (Analyze.get_ocaml_config_key "standard_library" bstate.bstate_config) in

  { compilation_modules  = modulesDeps
  ; compilation_csources = cbits.target_csources
  ; compilation_dag      = dag
  ; compilation_pp       = Pp.none
  ; compilation_filesdag = fdag
  ; compilation_builddir_c  = buildDir
  ; compilation_builddir_ml = (fun m ->
      match m with
      | Normal    -> buildDir
      | WithDebug -> buildDirD
      | WithProf  -> buildDirP)
  ; compilation_include_paths = (fun m hier ->
      ((match m with
          | Normal    -> buildDir
          | WithDebug -> buildDirD
          | WithProf  -> buildDirP) <//> Hier.to_dirpath hier) :: [autogenDir] @
      (List.map (fun dir -> dir <//> Hier.to_dirpath hier) obits.target_srcdir) @
      (match m with
       | Normal    -> depIncludePaths
       | WithDebug -> depIncludePathsD
       | WithProf  -> depIncludePathsP))
  ; compilation_linking_paths = [buildDir] @ depLinkingPaths
  ; compilation_linking_paths_p = [buildDirP;buildDir] @ depLinkingPaths
  ; compilation_linking_paths_d = [buildDirD;buildDir] @ depLinkingPaths
  ; compilation_c_include_paths = [cbits.target_cdir] @ cdepsIncludePaths @ [cCamlIncludePath; autogenDir]
  ; compilation_c_linking_paths = [buildDir]
  }

let prepare_target bstate buildDir target toplevelModules =
  try prepare_target_ bstate buildDir target toplevelModules
  with exn ->
    verbose Verbose "Prepare.target : uncaught exception %s\n%!" (Printexc.to_string exn);
    raise exn
