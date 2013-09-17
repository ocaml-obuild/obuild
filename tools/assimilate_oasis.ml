(* convert oasis file to obuild file *)
open Ext.Fugue
open Ext.Filepath
open Ext
open Obuild

let () =
    ignore(Filesystem.readFile (fp "_oasis"))
