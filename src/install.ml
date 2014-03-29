open Obuild
open Ext.Fugue
open Ext.Filepath
open Printf
open Modname
open Project
open Types
open Meta
open Target
open Helper
open Gconf

let opam_install_file projFile =
    let installPath = Dist.getDistPath () </> (fn projFile.name <.> "install") in
    Utils.generateFile installPath (fun add ->
        let cat_of_target target =
            match target.target_name with
            | ExeName _     -> Some "bin"
            | LibName _     -> Some "lib"
            | TestName _    -> None
            | BenchName _   -> None
            | ExampleName _ -> None
            in

        let allTargets = Project.get_all_buildable_targets projFile in
        List.iter (fun target ->
            match cat_of_target target with
            | None -> ()
            | Some cat ->
                add (sprintf "%s: [\n" cat);
                (* FIXME *)
                List.iter (fun file ->
                    add (sprintf "  %s\n" file)
                ) [];
                add ("\n");
        ) allTargets
    )

let lib_to_meta projFile lib =
    let requires_of_lib lib =
        let deps = lib.lib_target.target_obits.target_builddeps in
        [ (None, List.map (fun d -> fst d) deps) ]
        in
    let set_meta_field_from_lib pkg lib =
        { pkg with
              package_requires    = requires_of_lib lib
            ; package_description = if lib.lib_description <> "" then lib.lib_description else projFile.description
            ; package_archives    =
                [ ([Pred_Byte]  , fn_to_string (cmca_of_lib ByteCode Normal lib.lib_name))
                ; ([Pred_Native], fn_to_string (cmca_of_lib Native Normal lib.lib_name))
                ]
        }
        in

    let subPkgs =
        List.map (fun sub ->
            let npkg = newPkg (list_last (lib_name_to_string_nodes sub.lib_name)) in
            set_meta_field_from_lib npkg sub
        ) lib.lib_subs
        in

    let pkg = set_meta_field_from_lib (newPkg "") lib in
    { pkg with
          package_version          = projFile.version
        ; package_subs             = subPkgs
    }

let list_target_files_pred target pred =
  let build_dir = Dist.getBuildDest (Dist.Target target.Target.target_name) in
  Build.sanity_check build_dir target;
  (* don't play with matches *)
  let matches = Ext.Filesystem.list_dir_pred pred build_dir in
  (build_dir, matches)

let list_lib_files lib build_dir = list_target_files_pred lib (fun f ->
    if (fn_to_string f) = "META" then true
    else
      match Filetype.get_extension_path (build_dir </> f) with
      | Filetype.FileCMX | Filetype.FileCMO | Filetype.FileCMI | Filetype.FileA
      | Filetype.FileCMXA | Filetype.FileCMA | Filetype.FileCMTI -> true
      | _                  -> false)

let write_lib_meta projFile lib =
    let dir = Dist.getBuildDest (Dist.Target lib.lib_target.target_name) in
    let metadir_path = dir </> fn "META" in
    let pkg = lib_to_meta projFile lib in
    Meta.write metadir_path pkg

let copy_files files dest_dir dir_name =
  List.iter (fun (build_dir, build_files) ->
      List.iter (fun build_file ->
          Ext.Filesystem.copy_file (build_dir </> build_file) ((dest_dir </> dir_name) </> build_file)
        ) build_files;
    ) files

let install_lib proj_file lib dest_dir =
  write_lib_meta proj_file lib;
  let all_files = List.map (fun target ->
      let build_dir = Dist.getBuildDest (Dist.Target target.Target.target_name) in
      Build.sanity_check build_dir target;
      list_lib_files target build_dir
    ) (Project.lib_to_targets lib) in
  let dir_name = fn (lib_name_to_string lib.Project.lib_name) in
  verbose Report "installing library %s\n" (lib_name_to_string lib.Project.lib_name);
  verbose Debug "installing files: %s\n" (Utils.showList ","
                                            fn_to_string (List.concat (List.map snd all_files)));
  copy_files all_files dest_dir dir_name

let install_libs proj_file destdir =
  List.iter (fun lib -> install_lib proj_file lib destdir) proj_file.Project.libs;
