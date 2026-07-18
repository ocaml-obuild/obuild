open Helper
open Gconf
open Compat

(* Child processes run with stdout/stderr redirected to temporary files, and
 * completion is detected by polling waitpid WNOHANG.  This is deliberately
 * simpler than pipe-based multiplexing:
 * - no pipe-buffer deadlock class: children can never block on a full pipe
 * - no Unix.select on pipes, which native Windows does not support
 * - output is only ever consumed after completion (see terminate), so
 *   nothing is lost by not streaming
 *)

type t = {
  _args : string list; (* command args - kept for documentation *)
  pid : int; (* process PID *)
  time : float; (* process starting time *)
  out_file : string; (* stdout redirection file *)
  err_file : string; (* stderr redirection file *)
  mutable status : Unix.process_status option; (* set once the pid is reaped *)
}

(* create a new process with stdout and stderr redirected
 * and returns a new process_state
 *)
let make args =
  let escape s = try
      let _ = String.index s ' ' in "\"" ^ s ^ "\""
    with Not_found -> s in
  if Gconf.console.Gconf.verbosity >= Gconf.Trace then
    log Trace "  [CMD]: %s\n%!" (String.concat " " (List.map escape args));
  let out_file = Filename.temp_file "obuild" ".out" in
  let err_file = Filename.temp_file "obuild" ".err" in
  let fd_out = Unix.openfile out_file [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  let fd_err = Unix.openfile err_file [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  let argv = Array.of_list args in
  let pid = Unix.create_process argv.(0) argv Unix.stdin fd_out fd_err in
  Unix.close fd_out;
  Unix.close fd_err;
  {
    _args = args;
    pid = pid;
    time = Unix.gettimeofday ();
    out_file = out_file;
    err_file = err_file;
    status = None;
  }

type result = Success of string (* stdout *) * string (* stderr *) * float (* duration *)
            | Failure of string (* stdout *) * string (* stderr *) * int (* exit code *)

type call = unit -> t

(* poll interval while waiting for a child to finish; children are compiler
 * invocations that run for tens of milliseconds at minimum, so a couple of
 * milliseconds of completion latency is noise *)
let poll_interval = 0.002

(* process a list of processes until one finishes *)
let wait processes =
  (* true when the process has terminated; reaps and caches its status *)
  let is_finished (_, p) =
    match p.status with
    | Some _ -> true
    | None -> (
        match Unix.waitpid [ Unix.WNOHANG ] p.pid with
        | 0, _ -> false
        | _, st ->
            p.status <- Some st;
            true)
  in
  let remove_from_list e list = List.filter (fun x -> x <> e) list in
  let rec poll () =
    try
      let finished = List.find is_finished processes in
      (finished, remove_from_list finished processes)
    with Not_found ->
      sleepf poll_interval;
      poll ()
  in
  poll ()

(* cleanup a process and return a Success|Failure value.
 *)
let terminate (_, p) =
  let pstat =
    match p.status with
    | Some st -> st
    | None ->
        let _, st = Unix.waitpid [] p.pid in
        st
  in
  let slurp f =
    let content = try Filesystem.read_file (Filepath.fp f) with _ -> "" in
    (try Sys.remove f with Sys_error _ -> ());
    content
  in
  let stdout = slurp p.out_file in
  let stderr = slurp p.err_file in
  match pstat with
  | Unix.WEXITED 0   -> Success (stdout, stderr, Unix.gettimeofday () -. p.time)
  | Unix.WEXITED n   -> Failure (stdout, stderr, n)
  | Unix.WSIGNALED n -> Failure (stdout, stderr, 128 + n)
  | Unix.WSTOPPED n  -> Failure (stdout, stderr, 128 + n)

(* simple helper for a single process spawn|process|terminate *)
let run args =
  let p = make args in
  terminate ((), p)
