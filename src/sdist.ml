open Ext.Fugue
open Ext.Filepath
open Ext
open Obuild.Helper
open Obuild.Target
open Obuild.Gconf
open Obuild

let run projFile isSnapshot =
    let name = projFile.Project.name in
    let ver = projFile.Project.version in
    let sdistDir = name ^ "-" ^ ver in
    let sdistName = fn (sdistDir ^ ".tar.gz") in

    let dest = Dist.get_path () </> fn sdistDir in
    let currentDir = Unix.getcwd () in
    let _ = Filesystem.mkdirSafe dest 0o755 in

    (* copy project file and extra source files *)
    Filesystem.copy_to_dir (Project.findPath ()) dest;
    maybe_unit (fun src -> Filesystem.copy_to_dir src dest) projFile.Project.license_file;

    (* copy all libs modules *)
    let copy_obits obits =
      List.iter (fun dir ->
          Filesystem.iterate (fun ent -> 
              let fpath = dir </> ent in
              match Filetype.of_filepath fpath with
              | Filetype.FileML | Filetype.FileMLI -> Filesystem.copy_to_dir fpath dest
              | _                -> ()
            ) dir) obits.target_srcdir
        in
    let copy_cbits cbits =
        Filesystem.iterate (fun ent -> 
            let fpath = cbits.target_cdir </> ent in
            match Filetype.of_filepath fpath with
            | Filetype.FileC | Filetype.FileH -> Filesystem.copy_to_dir fpath dest
            | _ -> ()
        ) cbits.target_cdir
        in
        
    let copy_target target =
        copy_obits target.target_obits;
        copy_cbits target.target_cbits;
        ()
        in
    let copy_lib lib = List.iter copy_target (Project.Library.to_targets lib) in
    List.iter copy_lib projFile.Project.libs;
    List.iter (fun exe -> copy_target (Project.Executable.to_target exe)) projFile.Project.exes;
    List.iter (fun extra -> Filesystem.copy_to_dir extra dest) projFile.Project.extra_srcs;

    finally (fun () ->
        Unix.chdir (fp_to_string (Dist.get_path ()));
        Prog.runTar (fn_to_string sdistName) sdistDir
    ) (fun () -> Unix.chdir currentDir);

    verbose Report "Source tarball created: %s\n" (fp_to_string (Dist.get_path () </> sdistName));
    ()
