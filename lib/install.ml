open Fugue
open Filepath
open Printf
open Project
open Types
open Target
open Helper
open Gconf

let list_target_files_pred conf target pred =
  let build_dir = Dist.get_build_exn (Dist.Target target.Target.target_name) in
  Build.sanity_check conf build_dir target;
  (* only files: the build directory also holds sub-directories (opt-d,
     web-js, ...) whose extension-less names look like executables *)
  let matches =
    Filesystem.list_dir_pred (fun f -> pred f && not (Filesystem.is_dir (build_dir </> f))) build_dir
  in
  (build_dir, matches)

(* JavaScript runtime files of a library, installed beside its archives and
   declared as [jsoo_runtime] in its META (the convention js_of_ocaml
   consumers read).  wasm-files have no META convention and stay
   project-local. *)
let lib_js_runtime_names target =
  List.map (fun p -> fn_to_string (path_basename p)) target.Target.target_js.Target.web_files

let stage_js_runtime target =
  let build_dir = Dist.get_build_exn (Dist.Target target.Target.target_name) in
  List.iter
    (fun p -> Filesystem.copy_file p (build_dir </> path_basename p))
    target.Target.target_js.Target.web_files

let list_lib_files conf lib build_dir =
  let js_runtime = lib_js_runtime_names lib in
  list_target_files_pred conf lib (fun f ->
      if fn_to_string f = "META" || List.mem (fn_to_string f) js_runtime then
        true
      else
        match Filetype.of_filepath (build_dir </> f) with
        | Filetype.FileCMX
        | Filetype.FileCMI
        | Filetype.FileA
        | Filetype.FileCMXS
        | Filetype.FileCMXA
        | Filetype.FileCMA
        | Filetype.FileCMT
        | Filetype.FileCMTI -> true
        | _ -> false)

let list_exe_files conf lib build_dir =
  list_target_files_pred conf lib (fun f ->
      match Filetype.of_filepath (build_dir </> f) with
      | Filetype.FileEXE -> true
      | _ -> false)

(* the bytecode C-stub shared library (dll<clib>.so). It belongs in the toolchain's
   stublibs directory (on ld.conf), NOT the package dir, so bytecode/toplevel/Dynlink
   consumers can load it — the static libstubs_<clib>.a already covers native linking. *)
let list_stublib_files conf lib build_dir =
  list_target_files_pred conf lib (fun f ->
      match Filetype.of_filepath (build_dir </> f) with
      | Filetype.FileSO -> true
      | Filetype.FileOther "dll" -> true (* Windows stub: dll<clib>.dll *)
      | _ -> false)

let opam_install_file conf proj_file flags =
  let install_path = fp (proj_file.name ^ ".install") in
  Utils.generateFile install_path (fun add ->
      let all_targets = Project.get_all_installable_targets proj_file flags in
      let print_target_files target list_files_fun dest_prefix =
        let build_dir = Dist.get_build_exn (Dist.Target target.Target.target_name) in
        let _, files = list_files_fun target build_dir in
        List.iter
          (fun file ->
            let file_str = fn_to_string file in
            add (sprintf "  \"%s/%s\" {\"%s%s\"}\n"
              (fp_to_string build_dir) file_str dest_prefix file_str))
          files
      in
      (* A library's findlib package is its ROOT node (the first, un-dotted part
         of a possibly-dotted name); a sub-library shares its parent's root and so
         installs flat *beside* the parent — the generated META references sub-package
         archives with no `directory`, i.e. relative to the parent dir.

         The library whose root == the package name is the PRIMARY findlib package.
         opam's `lib:` section already scopes to %{lib}%/<pkg>/, so the primary (and
         its sub-libraries) install flat there. Every OTHER top-level library is a
         findlib package in its own right and goes to `lib_root:` under %{lib}%/<root>/,
         mirroring the direct-install layout (`obuild install`) — so e.g. obuild_core
         becomes a real, discoverable findlib package rather than an unreachable
         %{lib}%/obuild/obuild_core/. Keying on the full dotted name (the earlier bug)
         instead buried the primary at %{lib}%/<pkg>/<pkg>/ and broke every dependant. *)
      let lib_root l = List.hd (Libname.to_string_nodes l) in
      add "lib: [\n";
      List.iter
        (fun target ->
          match target.target_name with
          | Name.Lib l when lib_root l = proj_file.name ->
              print_target_files target (list_lib_files conf) ""
          | _ -> ())
        all_targets;
      add "]\n";
      add "lib_root: [\n";
      List.iter
        (fun target ->
          match target.target_name with
          | Name.Lib l when lib_root l <> proj_file.name ->
              print_target_files target (list_lib_files conf) (lib_root l ^ "/")
          | _ -> ())
        all_targets;
      add "]\n";
      add "bin: [\n";
      List.iter
        (fun target ->
          match target.target_name with
          | Name.Exe _ when not (Target.is_web target) ->
              print_target_files target (list_exe_files conf) ""
          | _ -> ())
        all_targets;
      add "]\n";
      (* opam's stublibs: section installs to %{lib}%/stublibs/ (on ld.conf) *)
      add "stublibs: [\n";
      List.iter
        (fun target ->
          match target.target_name with
          | Name.Lib _ -> print_target_files target (list_stublib_files conf) ""
          | _ -> ())
        all_targets;
      add "]\n")

let lib_to_meta conf proj_file lib =
  let requires_of_lib lib =
    let deps = lib.Library.target.target_obits.target_builddeps in
    [ ([], List.map (fun d -> fst d) deps) ]
  in
  let set_meta_field_from_lib pkg lib =
    let linkopts_of_lib lib =
      match lib.Library.target.Target.target_cstubs with
      | Some _ ->
          (* The installed stubs library is named libstubs_<library_name>.a *)
          (* Use -cclib to wrap C linker flags for ocamlfind/ocamlopt compatibility *)
          let lib_name = Libname.to_string lib.Library.name in
          [(None, "-cclib -lstubs_" ^ lib_name)]
      | None -> []
    in
    {
      pkg with
      Meta.Pkg.requires = requires_of_lib lib;
      Meta.Pkg.description =
        (if lib.Library.description <> "" then lib.Library.description else proj_file.description);
      Meta.Pkg.linkopts = linkopts_of_lib lib;
      Meta.Pkg.assignment =
        (match lib_js_runtime_names lib.Library.target with
         | [] -> []
         | names -> [ ("jsoo_runtime", String.concat " " names) ]);
      Meta.Pkg.archives =
        ([
           ([ Meta.Predicate.Byte ], fn_to_string (Libname.to_cmca ByteCode Normal lib.Library.name));
           ( [ Meta.Predicate.Byte; Meta.Predicate.Plugin ],
             fn_to_string (Libname.to_cmca ByteCode Normal lib.Library.name) );
           ([ Meta.Predicate.Native ], fn_to_string (Libname.to_cmca Native Normal lib.Library.name));
         ]
        @
        if Gconf.get_target_option_typed conf Gconf.Library_plugin then
          [
            ( [ Meta.Predicate.Native; Meta.Predicate.Plugin ],
              fn_to_string (Libname.to_cmxs Normal lib.Library.name) );
          ]
        else
          []);
    }
  in
  let subPkgs =
    List.map
      (fun sub ->
        let npkg = Meta.Pkg.make (list_last (Libname.to_string_nodes sub.Library.name)) in
        set_meta_field_from_lib npkg sub)
      lib.Library.subs
  in
  let pkg = set_meta_field_from_lib (Meta.Pkg.make "") lib in
  { pkg with Meta.Pkg.version = proj_file.version; Meta.Pkg.subs = subPkgs }

let write_lib_meta conf projFile lib =
  let dir = Dist.get_build_exn (Dist.Target lib.Library.target.target_name) in
  let metadir_path = dir </> fn "META" in
  let pkg = lib_to_meta conf projFile lib in
  List.iter stage_js_runtime (Project.Library.to_targets lib);
  Meta.Pkg.write metadir_path pkg

let copy_files files dest_dir dir_name =
  List.iter
    (fun (build_dir, build_files) ->
      List.iter
        (fun build_file ->
          Filesystem.copy_file (build_dir </> build_file) (dest_dir </> dir_name </> build_file))
        build_files)
    files

let install_lib conf proj_file lib dest_dir =
  write_lib_meta conf proj_file lib;
  let all_files =
    List.map
      (fun target ->
        let build_dir = Dist.get_build_exn (Dist.Target target.Target.target_name) in
        Build.sanity_check conf build_dir target;
        list_lib_files conf target build_dir)
      (Project.Library.to_targets lib)
  in
  let stub_files =
    List.map
      (fun target ->
        let build_dir = Dist.get_build_exn (Dist.Target target.Target.target_name) in
        list_stublib_files conf target build_dir)
      (Project.Library.to_targets lib)
  in
  let dir_name = fn (Libname.to_string lib.Project.Library.name) in
  log Report "installing library %s\n" (Libname.to_string lib.Project.Library.name);
  log Debug "installing files: %s\n"
    (Utils.showList "," fn_to_string (List.concat (List.map snd all_files)));
  copy_files all_files dest_dir dir_name;
  (* stub shared libs go to <root>/stublibs/, not the package dir *)
  if List.exists (fun (_, fs) -> fs <> []) stub_files then
    copy_files stub_files dest_dir (fn "stublibs")

let install_exe conf exe bindir =
  let target = Project.Executable.to_target exe in
  let build_dir = Dist.get_build_exn (Dist.Target target.Target.target_name) in
  Build.sanity_check conf build_dir target;
  let _, files = list_exe_files conf target build_dir in
  log Report "installing executable %s\n" exe.Project.Executable.name;
  Filesystem.mkdir_safe_recursive bindir 0o755 ;
  List.iter
    (fun file ->
      let src = build_dir </> file in
      let dst = bindir </> file in
      Filesystem.copy_file src dst;
      Unix.chmod (fp_to_string dst) 0o755)
    files

let install conf proj_file destdir bindir opam =
  if not opam then begin
    List.iter (fun lib -> install_lib conf proj_file lib destdir) proj_file.Project.libs;
    (* only install executables that were actually built *)
    List.iter
      (fun exe ->
        let target = Project.Executable.to_target exe in
        if Target.is_web target then
          log Report "skipping web executable %s (web outputs are not installed)\n"
            exe.Project.Executable.name
        else if Filesystem.is_dir (Dist.get_build_path (Dist.Target target.Target.target_name)) then
          install_exe conf exe bindir)
      proj_file.Project.exes
  end else
    List.iter (fun lib -> write_lib_meta conf proj_file lib) proj_file.Project.libs
