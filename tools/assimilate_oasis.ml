(* convert oasis file to obuild file *)
open Ext.Fugue
open Ext.Filepath
open Ext
open Obuild

let () =
    let content = Filesystem.readFile (fp "_oasis") in
    ()
