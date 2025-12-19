(** Task dependency tracking for build scheduling *)

(** Dependency traversal direction *)
type direction =
  | FromChildren  (** Start from leaves, move toward roots *)
  | FromParent    (** Start from roots, move toward leaves *)

(** Task dependency tracker *)
type 'a t

val init : ?direction:direction -> 'a Dag.t -> 'a t
(** [init ~direction dag] initializes task dependency tracker from DAG

    Creates a tracker that will iterate through the DAG in the specified
    direction. Defaults to FromChildren (bottom-up).

    @param direction traversal direction (default: FromChildren)
    @param dag dependency graph
    @return task dependency tracker *)

val init_with : 'a Dag.t -> direction -> 'a list -> 'a t
(** [init_with dag direction nodes] initializes tracker with specific starting nodes

    @param dag dependency graph
    @param direction traversal direction
    @param nodes initial tasks to process
    @return task dependency tracker *)

val get_next : 'a t -> (int * 'a) option
(** [get_next taskdep] retrieves next task ready for execution

    Returns the next task whose dependencies are satisfied.
    The int is a sequential task index for tracking.

    @param taskdep task tracker
    @return Some (task_index, task) if available, None if no tasks ready *)

val mark_done : 'a t -> 'a -> unit
(** [mark_done taskdep task] marks task as completed

    Updates the tracker to record task completion and makes
    dependent tasks available if their dependencies are now satisfied.

    @param taskdep task tracker
    @param task completed task *)

val is_complete : 'a t -> bool
(** [is_complete taskdep] checks if all tasks are done

    @param taskdep task tracker
    @return true if all tasks in DAG are completed *)

val linearize : 'a Dag.t -> direction -> 'a list -> 'a list
(** [linearize dag direction nodes] computes linear execution order

    Performs topological sort of the DAG from specified nodes
    in the given direction.

    @param dag dependency graph
    @param direction traversal direction
    @param nodes starting nodes
    @return tasks in linear execution order *)

val dump : ('a -> string) -> 'a t -> unit
(** [dump to_string taskdep] prints debug information

    Outputs completed tasks and pending tasks for debugging.

    @param to_string function to convert task to string
    @param taskdep task tracker *)
