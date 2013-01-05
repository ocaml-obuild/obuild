open Types
open Printf

let getGeneralConfig () =
    { conf_verbosity = Report 
    ; conf_withopt   = true
    ; conf_setup     = []
    }

let print_warnings warnings =
    if warnings <> "" then fprintf stderr "%s\n%!" warnings else ()

let verbose generalConf lvl fmt =
    if lvl <= generalConf.conf_verbosity
        then printf fmt
        else ifprintf stdout fmt

type execState = Success of string * string | Failure of string

type process_state =
    { outbuf : Buffer.t
    ; errbuf : Buffer.t
    ; fd_out : Unix.file_descr
    ; fd_err : Unix.file_descr
    ; pid    : int
    ; mutable out_closed : bool
    ; mutable err_closed : bool
    }

(* create a new process with stdout and stderr redirected
 * and returns a new process_state
 *)
let spawn gconf args =
    verbose gconf DebugPlus "  [CMD]: %s\n%!" (String.concat " " args);
    let (r1,w1) = Unix.pipe () in
    let (r2,w2) = Unix.pipe () in
    let argv = Array.of_list args in
    let pid = Unix.create_process argv.(0) argv Unix.stdin w1 w2 in
    List.iter Unix.close [w1;w2];
    { outbuf = Buffer.create 1024
    ; errbuf = Buffer.create 1024
    ; fd_out = r1
    ; fd_err = r2
    ; pid    = pid
    ; out_closed = false
    ; err_closed = false
    }

(* process a set of processes states until one process
 * finished. the finishing 'signal' is when both stdout
 * and stderr are eofed.
 *)
let process_processes states =
    let st_finished (_, st) = st.err_closed && st.out_closed in
    let list_remove e list = List.filter (fun x -> x <> e) list in
    let process_loop () =
        let b = String.create 1024 in
        let liveStates = ref states in
        let doneState = ref None in
        let getReadFds () =
            List.concat (List.map (fun (_, st) ->
                  (if st.out_closed then [] else [ st.fd_out ])
                @ (if st.err_closed then [] else [ st.fd_err ])
            ) !liveStates)
            in
        let readFds = ref (getReadFds ()) in
        (* process until at least one process terminate *)
        while !doneState = None
        do
            let (rs, _, _) = Unix.select !readFds [] [] 2.0 in
            List.iter (fun (task, st) ->
                if not st.out_closed && List.mem st.fd_out rs then (
                    let nb = Unix.read st.fd_out b 0 1024 in
                    if nb > 0
                        then Buffer.add_substring st.outbuf b 0 nb
                        else (Unix.close st.fd_out; st.out_closed <- true; readFds := getReadFds ())
                );
                if not st.err_closed && List.mem st.fd_err rs then (
                    let nb = Unix.read st.fd_err b 0 1024 in
                    if nb > 0
                        then Buffer.add_substring st.errbuf b 0 nb
                        else (Unix.close st.fd_err; st.err_closed <- true; readFds := getReadFds ())
                );
                (if !doneState = None && st_finished (task,st)
                    then doneState := Some (task,st)
                );
            ) !liveStates;
        done;
        match !doneState with
        | None        -> assert false
        | Some dState -> (dState, list_remove dState !liveStates)
        in

    try
        let doneSt = List.find st_finished states in
        (doneSt, list_remove doneSt states)
    with Not_found ->
        process_loop ()

(* cleanup a process and return a Success|Failure value.
 *)
let terminate_process (_, st) =
    let (_, pstat) = Unix.waitpid [] st.pid in
    match pstat with
    | Unix.WEXITED 0 -> Success (Buffer.contents st.outbuf, Buffer.contents st.errbuf)
    | Unix.WEXITED n -> Failure (Buffer.contents st.errbuf)
    | _              -> Failure ""

(* simple helper for a single process spawn|process|terminate *)
let run_with_outputs gconf args =
    let st = spawn gconf args in
    let (st2, _) = process_processes [((), st)] in
    terminate_process st2

(* this is used to control the scheduler behavior
 * from the idle function *)
type 'a schedule_op = Terminate
                    | WaitingProcess
                    | AddProcess of ('a * process_state)
                    | Retry

let schedule_op_to_string op =
    match op with
    | Terminate        -> "terminate"
    | WaitingProcess   -> "waiting-process"
    | AddProcess (_,_) -> "add-process"
    | Retry            -> "retry"

(* this is a simple one thread loop to schedule
 * multiple tasks (forked) until they terminate
 *
 * the idle function is called when there's capacity in the runqueue for
 * another process.
 *
 * the finish function is called when a task has finished
 **)
let schedule j idle finish =
    let runqueue = ref [] in
    let terminate = ref false in
    let waitingProcess = ref false in
    while not !terminate || !runqueue <> [] do
        while not !terminate && not !waitingProcess && List.length !runqueue < j do
            let rec retry_idle () =
                match idle () with
                | Retry          -> retry_idle ()
                | AddProcess st  -> runqueue := st :: !runqueue
                | WaitingProcess -> waitingProcess := true
                | Terminate      -> terminate := true
                in
            retry_idle ()
        done;

        if List.length !runqueue > 0
            then
                let (doneSt, newSts) = process_processes !runqueue in
                waitingProcess := false;
                runqueue := newSts;
                finish doneSt
            else
                assert (!terminate)
    done;
    ()
