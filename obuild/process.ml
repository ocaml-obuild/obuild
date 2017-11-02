open Helper
open Gconf
open Ext.Compat
       
type output = {
  buf : Buffer.t;
  fd : Unix.file_descr;
  mutable closed : bool;
}

let create_output fd = {
  buf = Buffer.create 1024;
  fd = fd;
  closed = false;
}

type t = {
  args : string list; (* command args *)
  pid : int; (* process PID *)
  time : float; (* Process starting time *)
  out : output;
  err : output;
}

(* create a new process with stdout and stderr redirected
 * and returns a new process_state
 *)
let make args =
  let escape s = try
      let _ = String.index s ' ' in "\"" ^ s ^ "\""
    with Not_found -> s in
  verbose DebugPlus "  [CMD]: %s\n%!" (String.concat " " (List.map escape args));
  let (r1,w1) = Unix.pipe () in
  let (r2,w2) = Unix.pipe () in
  let argv = Array.of_list args in
  let pid = Unix.create_process argv.(0) argv Unix.stdin w1 w2 in
  List.iter Unix.close [w1;w2];
  {
    args = args;
    out = create_output r1;
    err = create_output r2;
    pid    = pid;
    time = Unix.gettimeofday ();
  }

type result = Success of string (* stdout *) * string (* stderr *) * float (* duration *)
            | Failure of string (* sterr *)

type call = unit -> t

(* process a list of processes until one finish.
 * The finishing 'signal' is when both stdout
 * and stderr are eofed. *)
let wait processes =
  let is_finished (_, p) = p.err.closed && p.out.closed in
  let remove_from_list e list = List.filter (fun x -> x <> e) list in
  let process_loop () =
    let b = bytes_create 1024 in
    let live_processes = ref processes in
    let done_processes = ref None in
    let read_fds () = List.fold_left (fun acc (_, p) ->
        let res = if p.out.closed then acc else p.out.fd :: acc in
        if p.err.closed then res else p.err.fd :: res) [] !live_processes in
    let fds = ref (read_fds ()) in
   (* process until at least one process terminate *)
    while !done_processes = None do
      let (reads, _, _) = Unix.select !fds [] [] 2.0 in
      let check_fd out =
        if not out.closed && List.mem out.fd reads then
          let nb = Unix.read out.fd b 0 1024 in
          if nb > 0
          then buffer_add_subbytes out.buf b 0 nb
          else (Unix.close out.fd; out.closed <- true; fds := read_fds ())
      in
      List.iter (fun (task, p) ->
          check_fd p.out;
          check_fd p.err;
          if !done_processes = None && is_finished (task, p)
          then done_processes := Some (task, p)
        ) !live_processes;
    done;
    match !done_processes with
    | None -> assert false
    | Some finished -> (finished, remove_from_list finished !live_processes)
  in
  try
    let finished = List.find is_finished processes in
    (finished, remove_from_list finished processes)
  with Not_found -> process_loop ()

(* cleanup a process and return a Success|Failure value.
 *)
let terminate (_, p) =
  let (_, pstat) = Unix.waitpid [] p.pid in
  match pstat with
  | Unix.WEXITED 0 -> Success (Buffer.contents p.out.buf, Buffer.contents p.err.buf, Unix.gettimeofday () -. p.time)
  | _              -> Failure (Buffer.contents p.err.buf)

(* simple helper for a single process spawn|process|terminate *)
let run args =
  let p = make args in
  let (p2, _) = wait [((), p)] in
  terminate p2
