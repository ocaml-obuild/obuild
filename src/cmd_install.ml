open Ext.Filepath
open Obuild

let dest_dir = ref ""
let opam_install = ref false

let args = [
  ("--destdir", Arg.Set_string dest_dir, "DIR override destination where to install (default coming from findlib configuration)");
  ("--opam", Arg.Set opam_install, " only create the .install file for opam (do not copy the files)");
]

let mainInstall argv =
  Dist.exist ();
  let setup = App_utils.read_setup () in
  let proj_file = App_utils.project_read () in
  let flags = Configure.check proj_file false setup in
  let dest_dir =
    (if !dest_dir = ""
     then (match FindlibConf.get_destdir () with
         | None   -> failwith "no destdir specified, and no findlib default found"
         | Some p -> p
       )
     else fp !dest_dir)
  in
  (* install all the libs *)
  Install.install_libs proj_file dest_dir !opam_install;
  if !opam_install then
    Install.opam_install_file proj_file flags

let () =
  let cmd = {
    Cmd.name = "install";
    args = args;
    fn = mainInstall;
    short_desc = "Install this package";
    long_desc = "\
XXX
";
  } in
  Cmd.register_cmd cmd
