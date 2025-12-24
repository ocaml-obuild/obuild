(** DAG Utility Functions

    This module provides iteration and linearization utilities for DAGs (Directed Acyclic Graphs).
    It uses the Taskdep module to traverse DAGs in topological order.
 *)

(** Iterate over DAG nodes in topological order

    @param f Function to apply to each node
    @param dag The DAG to iterate over
 *)
val iter : ('a -> unit) -> 'a Dag.t -> unit

(** Iterate over DAG nodes with indices in topological order

    @param f Function to apply to each node with its index
    @param dag The DAG to iterate over
 *)
val iteri : (int -> 'a -> unit) -> 'a Dag.t -> unit

(** Linearize a DAG into a topologically sorted list

    @param dag The DAG to linearize
    @return List of nodes in topological order
 *)
val linearize : 'a Dag.t -> 'a list
