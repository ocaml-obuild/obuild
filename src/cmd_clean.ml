open Ext
open Obuild

let mainClean _ =
  if Filesystem.exists (Dist.get_path ())
  then begin
    Filesystem.removeDir (Dist.get_path ());
    Dist.remove_dead_links ()
  end

let () =
  let cmd = {
    Cmd.name = "clean";
    args = [];
    fn = mainClean;
    short_desc = "Clean up after a build";
    long_desc = "\
Remove all by-product of compilation (.cmx, .cmi, .cmo, etc)
and remove the dist directory.";
  } in
  Cmd.register_cmd cmd
