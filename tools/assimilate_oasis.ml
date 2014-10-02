(* convert oasis file to obuild file *)
open Ext.Filepath
open Ext

let () =
    ignore(Filesystem.readFile (fp "_oasis"))
