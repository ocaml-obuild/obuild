(*
 * track generation of files
 *)
open Filepath
open Types

(* sources generates targets *)
type trackgen =
    { source       : filepath
    ; dependencies : filepath list
    ; targets      : filepath list
    }

type mtime_tracker = (filepath, float) Hashtbl.t
type digest_tracker = (filepath, float) Hashtbl.t

let mtimetracker_init () = Hashtbl.create 64
let digesttracker_init () = Hashtbl.create 64

let getMTime mtimeTracker path =
    try Hashtbl.find mtimeTracker path
    with Not_found ->
        let mtime = Filesystem.getModificationTime path in
        Hashtbl.add mtimeTracker path mtime;
        mtime

let run_if_needed mtimeTracker trackgen noneed runned =
    let srcMtime = getMTime mtimeTracker trackgen.source in
    ()

