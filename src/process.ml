open Types
open Helper
open Gconf

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

type spawn = unit -> process_state

(* create a new process with stdout and stderr redirected
 * and returns a new process_state
 *)
let spawn args =
    let maybeEscape s = try let _ = String.index s ' ' in "\"" ^ s ^ "\"" with Not_found -> s in
    verbose DebugPlus "  [CMD]: %s\n%!" (String.concat " " (List.map maybeEscape args));
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
let run_with_outputs args =
    let st = spawn args in
    let (st2, _) = process_processes [((), st)] in
    terminate_process st2

(* this is used to control the scheduler behavior
 * from the idle function *)
type 'a schedule_op = Terminate
                    | WaitingTask
                    | AddProcess of ('a * process_state)
                    | AddTask of ('a * (spawn list list))
                    | Retry

let schedule_op_to_string op =
    match op with
    | Terminate        -> "terminate"
    | WaitingTask      -> "waiting-task"
    | AddProcess (_,_) -> "add-process"
    | AddTask    (_,_) -> "add-task"
    | Retry            -> "retry"

type 'a schedule_task_group =
    { mutable group_completion : int
    ; mutable group_next : ('a * spawn) list list
    }

type schedule_stats =
    { mutable max_runqueue : int
    ; mutable nb_processes : int
    }

(* this is a simple one thread loop to schedule
 * multiple tasks (forked) until they terminate
 *
 * the idle function is called when there's capacity in the runqueue for
 * another task.
 *
 * the finish function is called when a subtask of the task has finished.
 * if all the subtasks in the task are done then the second value is set
 * to true.
 **)
let schedule j idle finish =
    let runqueue = ref [] in
    let waitqueue = ref [] in
    let terminate = ref false in
    let waitingTask = ref false in
    let tasks = ref [] in

    let rec retry_idle () =
        match idle () with
        | Retry          -> retry_idle ()
        | AddProcess st  -> runqueue := st :: !runqueue
        | WaitingTask    -> waitingTask := true
        | Terminate      -> terminate := true
        | AddTask (t,ps) ->
            (match List.map (List.map (fun p -> (t, p))) ps with
            | []           -> failwith "internal error: empty task added to the scheduler"
            | psFirst::pss ->
                let tg =
                    { group_completion = List.length psFirst
                    ; group_next       = pss
                    }
                    in
                tasks := (t,tg) :: !tasks;
                waitqueue := psFirst @ !waitqueue;
            )
        in
    let stats =
        { max_runqueue = 0
        ; nb_processes = 0
        }
        in
    let incr_process () = stats.nb_processes <- stats.nb_processes + 1 in
    let set_max () =
        let m = List.length !runqueue in
        if stats.max_runqueue < m then (
            stats.max_runqueue <- m
        )
        in

    (* add more bulletproofing to prevent busy looping for no reason
     * if user of this api is not behaving properly *)
    while not !terminate || !runqueue <> [] || !waitqueue <> [] do
        while not !terminate && not !waitingTask && List.length !runqueue < j do
            match !waitqueue with
            | []        -> retry_idle ()
            | (t,p)::xs -> incr_process (); runqueue := (t,p ()) :: !runqueue; waitqueue := xs;
        done;
        set_max ();

        if List.length !runqueue > 0
            then
                let (doneSt, newSts) = process_processes !runqueue in
                let (doneTask,_) = doneSt in
                let finishedTask =
                    try
                        let tg = List.assoc doneTask !tasks in
                        tg.group_completion <- tg.group_completion - 1;
                        if tg.group_completion = 0
                            then (
                                match tg.group_next with
                                | [] -> 
                                    tasks := List.filter (fun (t,_) -> t <> doneTask) !tasks;
                                    true
                                | g :: gs ->
                                    tg.group_completion <- List.length g;
                                    tg.group_next <- gs;
                                    waitqueue := g @ !waitqueue;
                                    false
                            ) else
                                false
                    with Not_found ->
                        true
                    in
                waitingTask := false;
                runqueue := newSts;
                finish doneSt finishedTask
            else
                assert (!terminate)
    done;
    stats
