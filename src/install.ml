open Obuild
open Ext.Fugue
open Ext.Filepath
open Printf
open Project
open Types
open Target
open Helper
open Gconf

let list_target_files_pred target pred =
  let build_dir = Dist.get_build_exn (Dist.Target target.Target.target_name) in
  Build.sanity_check build_dir target;
  (* don't play with matches *)
  let matches = Ext.Filesystem.list_dir_pred pred build_dir in
  (build_dir, matches)

let list_lib_files lib build_dir = list_target_files_pred lib (fun f ->
    if (fn_to_string f) = "META" then true
    else
      match Filetype.of_filepath (build_dir </> f) with
      | Filetype.FileCMX | Filetype.FileCMI | Filetype.FileA | Filetype.FileCMXS
      | Filetype.FileCMXA | Filetype.FileCMA | Filetype.FileCMT | Filetype.FileCMTI -> true
      | _                  -> false)

let list_exe_files lib build_dir = list_target_files_pred lib (fun f ->
    match Filetype.of_filepath (build_dir </> f) with
    | Filetype.FileEXE -> true
    | _                -> false)

let opam_install_file proj_file flags =
  let install_path = fp (proj_file.name ^ ".install") in
  Utils.generateFile install_path (fun add ->
      let all_targets = Project.get_all_installable_targets proj_file flags in
      let print_target_files target list_files_fun =
        let build_dir = Dist.get_build_exn (Dist.Target target.Target.target_name) in
        let (_, files) = list_files_fun target build_dir in
        List.iter (fun file -> let file_str = fn_to_string file in
                    add (sprintf "  \"%s/%s\" {\"%s\"}\n" (fp_to_string build_dir) file_str file_str)
                  ) files
      in
      add (sprintf "%s: [\n" "lib");
      List.iter (fun target -> match target.target_name with
          | Name.Lib _ -> print_target_files target list_lib_files | _ -> ()) all_targets;
      add ("]\n");
      add (sprintf "%s: [\n" "bin");
      List.iter (fun target -> match target.target_name with
          | Name.Exe _ -> print_target_files target list_exe_files | _ -> ()) all_targets;
      add ("]\n");
    )

let lib_to_meta proj_file lib =
  let requires_of_lib lib =
    let deps = lib.Library.target.target_obits.target_builddeps in
    [ ([], List.map (fun d -> fst d) deps) ]
  in
  let set_meta_field_from_lib pkg lib = {
    pkg with Meta.Pkg.requires    = requires_of_lib lib;
             Meta.Pkg.description = if lib.Library.description <> "" then lib.Library.description else proj_file.description;
             Meta.Pkg.archives    = [
               ([Meta.Predicate.Byte]  , fn_to_string (Libname.to_cmca ByteCode Normal lib.Library.name));
               ([Meta.Predicate.Byte; Meta.Predicate.Plugin]  , fn_to_string (Libname.to_cmca ByteCode Normal lib.Library.name));
               ([Meta.Predicate.Native], fn_to_string (Libname.to_cmca Native Normal lib.Library.name))
             ] @ (if (Gconf.get_target_option "library-plugin") then
                    [([Meta.Predicate.Native; Meta.Predicate.Plugin], fn_to_string (Libname.to_cmxs Normal lib.Library.name))]
                  else [])
  } in
  let subPkgs = List.map (fun sub ->
      let npkg = Meta.Pkg.make (list_last (Libname.to_string_nodes sub.Library.name)) in
      set_meta_field_from_lib npkg sub
    ) lib.Library.subs
  in
  let pkg = set_meta_field_from_lib (Meta.Pkg.make "") lib in {
    pkg with Meta.Pkg.version          = proj_file.version;
             Meta.Pkg.subs             = subPkgs
  }

let write_lib_meta projFile lib =
    let dir = Dist.get_build_exn (Dist.Target lib.Library.target.target_name) in
    let metadir_path = dir </> fn "META" in
    let pkg = lib_to_meta projFile lib in
    Meta.Pkg.write metadir_path pkg

let copy_files files dest_dir dir_name =
  List.iter (fun (build_dir, build_files) ->
      List.iter (fun build_file ->
          Ext.Filesystem.copy_file (build_dir </> build_file) ((dest_dir </> dir_name) </> build_file)
        ) build_files;
    ) files

let install_lib proj_file lib dest_dir =
  write_lib_meta proj_file lib;
  let all_files = List.map (fun target ->
      let build_dir = Dist.get_build_exn (Dist.Target target.Target.target_name) in
      Build.sanity_check build_dir target;
      list_lib_files target build_dir
    ) (Project.Library.to_targets lib) in
  let dir_name = fn (Libname.to_string lib.Project.Library.name) in
  verbose Report "installing library %s\n" (Libname.to_string lib.Project.Library.name);
  verbose Debug "installing files: %s\n" (Utils.showList ","
                                            fn_to_string (List.concat (List.map snd all_files)));
  copy_files all_files dest_dir dir_name

let install_libs proj_file destdir opam =
  if not opam then
    List.iter (fun lib -> install_lib proj_file lib destdir) proj_file.Project.libs
  else
    List.iter (fun lib -> write_lib_meta proj_file lib) proj_file.Project.libs;
