(** Process execution and management *)

(** Process state *)
type t

(** Process execution result *)
type result =
  | Success of string * string * float  (** stdout, stderr, duration *)
  | Failure of string                   (** stderr *)

(** Delayed process creation *)
type call = unit -> t

val make : string list -> t
(** [make args] creates and starts a new process

    Creates a subprocess with stdout and stderr redirected to pipes.
    The first element of args is the program to execute.

    @param args command line arguments (program + args)
    @return process state *)

val wait : ('a * t) list -> ('a * t) * ('a * t) list
(** [wait processes] waits for one process to finish

    Monitors multiple processes and returns when one completes,
    reading from stdout and stderr until both are closed.

    @param processes list of (task, process) pairs
    @return ((task, finished_process), remaining_processes) *)

val terminate : ('a * t) -> result
(** [terminate (task, process)] cleans up process and returns result

    Waits for process to terminate and collects exit status.

    @param task process pair
    @return Success with outputs and duration, or Failure with stderr *)

val run : string list -> result
(** [run args] executes a single process synchronously

    Convenience function that combines make, wait, and terminate
    for simple single-process execution.

    @param args command line arguments (program + args)
    @return execution result *)
