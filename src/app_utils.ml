open Printf
open Obuild.Helper
open Obuild.Gconf
open Obuild

let read_setup () =
  FindlibConf.load ();
  let setup = Dist.read_setup () in
  (* all_options are restored from setup file *)
  Configure.set_opts setup;
  setup

let project_read () =
  try Project.read gconf.strict
  with exn -> verbose Verbose "exception during project read: %s\n" (Printexc.to_string exn);
    raise exn

let unimplemented () =
    eprintf "sorry, you've reached an unimplemented part ! please be patient or send a patch.\n";
    exit 1
