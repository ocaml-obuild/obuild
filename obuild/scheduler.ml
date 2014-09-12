type call = unit -> Process.t

(* this is used to control the scheduler behavior
 * from the idle function *)
type 'a t = Terminate
          | WaitingTask
          | AddProcess of ('a * Process.t)
          | AddTask of ('a * (call list list))
          | Retry
          | FinishTask of 'a

let to_string = function
  | Terminate          -> "terminate"
  | WaitingTask        -> "waiting-task"
  | AddProcess (_,_)   -> "add-process"
  | AddTask    (_,_)   -> "add-task"
  | Retry              -> "retry"
  | FinishTask _       -> "finish-task"
    
type 'a task_group = {
  mutable completion : int;
  mutable next : ('a * call) list list;
}

type stats = {
  mutable max_runqueue : int; 
  mutable nb_processes : int;
}

type 'a state = {
  mutable runqueue : ('a * Process.t) list;
  mutable waitqueue : ('a * call) list;
  mutable terminate : bool;
  mutable waiting_task : bool;
  mutable tasks : ('a * 'a task_group) list;
}

(* wait until a process finish. *)

let wait_process state =
  let (proc_done, processes) = Process.wait state.runqueue in
  let (task_done,_) = proc_done in
  let finished_task =
    try
      let tg = List.assoc task_done state.tasks in
      tg.completion <- tg.completion - 1;
      if tg.completion = 0
      then (
        match tg.next with
        | [] -> 
          state.tasks <- List.filter (fun (t,_) -> t <> task_done) state.tasks;
          true
        | g :: gs ->
          tg.completion <- List.length g;
          tg.next <- gs;
          state.waitqueue <- g @ state.waitqueue;
          false
      ) else
        false
    with Not_found ->
      true
  in
  state.runqueue <- processes;
  (proc_done, finished_task)

let rec idle_loop idle_fun on_task_finish_fun state =
  match idle_fun () with
  | Retry          -> idle_loop idle_fun on_task_finish_fun state
  | AddProcess p   -> state.runqueue <- p :: state.runqueue
  | WaitingTask    -> state.waiting_task <- true
  | Terminate      -> state.terminate <- true
  | FinishTask t   -> on_task_finish_fun t; (* retry *) idle_loop idle_fun on_task_finish_fun state
  | AddTask (t,ps) ->
    (match List.map (List.map (fun p -> (t, p))) ps with
     | []           -> failwith "internal error: empty task added to the scheduler"
     | first::pss ->
       let tg = { 
         completion = List.length first;
         next       = pss
       } in
       state.tasks <- (t,tg) :: state.tasks;
       state.waitqueue <- first @ state.waitqueue;
    )

(* when the scheduler has some room, we get the next task from
 * taskdep and either start a process or call retry.
 *
 * Retry is returned when no process need to be spawned for the next task
 * since the dependencies have not changed and thus the cache still have
 * valid target file. Instead of returning retry, we could just go get
 * the next task ourself.
 *)
let schedule_idle taskdep dispatch_fun () =
  if Taskdep.is_complete taskdep
  then Terminate
  else match Taskdep.get_next taskdep with
    | None      -> WaitingTask
    | Some task -> dispatch_fun task
    
(* this is a simple one thread loop to schedule
 * multiple tasks (forked) until they terminate
 *
 * the idle_fun function is called when there's capacity in the runqueue for
 * another task.
 *
 * the finish function is called when a subtask of the task has finished.
 * if all the subtasks in the task are done then the second value is set
 * to true.
 **)
let schedule j taskdep dispatch_fun finish_fun =
  let st = { 
    runqueue = [];
    waitqueue = [];
    terminate = false;
    waiting_task = false;
    tasks = [];
  } in
  let on_task_finish task = Taskdep.mark_done taskdep task in
  let stats = { max_runqueue = 0; nb_processes = 0 } in
  let pick_process (task, process) remaining_processes =
    stats.nb_processes <- stats.nb_processes + 1;
    st.runqueue <- (task,process ()) :: st.runqueue; 
    st.waitqueue <- remaining_processes 
  in
  let set_max () =
    let m = List.length st.runqueue in
    if stats.max_runqueue < m then stats.max_runqueue <- m
  in

  (* add more bulletproofing to prevent busy looping for no reason
   * if user of this api is not behaving properly *)
  while not st.terminate || st.runqueue <> [] || st.waitqueue <> [] do
    while not st.terminate && not st.waiting_task && List.length st.runqueue < j do
      match st.waitqueue with
      | []           -> idle_loop (schedule_idle taskdep dispatch_fun) on_task_finish st
      | (t,p)::procs -> pick_process (t,p) procs
    done;
    set_max ();

    if List.length st.runqueue > 0
    then
      let (proc_done, finished_task) = wait_process st in
      st.waiting_task <- false;
      finish_fun proc_done finished_task
    else
      assert (st.terminate)
  done;
  stats
