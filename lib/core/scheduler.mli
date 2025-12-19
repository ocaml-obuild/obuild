(** Task scheduler for parallel build execution *)

(** Delayed process call *)
type call = unit -> Process.t

(** Scheduler control actions returned by idle/dispatch functions *)
type 'a t =
  | Terminate                        (** Terminate scheduling *)
  | WaitingTask                      (** Waiting for a task to become available *)
  | AddProcess of ('a * Process.t)   (** Add a process to the run queue *)
  | AddTask of ('a * (call list list))  (** Add a new task with process groups *)
  | Retry                            (** Retry scheduling *)
  | FinishTask of 'a                 (** Mark task as finished *)

val to_string : 'a t -> string
(** Convert scheduler action to human-readable string *)

(** Scheduler statistics *)
type stats = {
  mutable max_runqueue : int;  (** Maximum run queue size reached *)
  mutable nb_processes : int;  (** Total number of processes executed *)
}

val schedule : int -> 'a Taskdep.t -> (int * 'a -> 'a t) -> (('a * Process.t) -> bool -> unit) -> stats
(** [schedule j taskdep dispatch_fun finish_fun] runs the parallel scheduler

    The scheduler manages parallel execution of tasks using a simple
    multi-process model with configurable parallelism.

    @param j maximum number of parallel jobs
    @param taskdep task dependency tracker
    @param dispatch_fun function to dispatch next task (returns control action)
    @param finish_fun function called when a process completes
    @return scheduler statistics *)
