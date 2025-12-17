(** Directed Acyclic Graph (DAG) implementation

    This module provides a bi-directional DAG where each node maintains
    references to both its parents and children for efficient traversal. *)

(** The type of a DAG with nodes of type ['a] *)
type 'a t

(** Internal node structure - abstract type for encapsulation *)
type 'a dagnode

(** {1 Exceptions} *)

exception DagNode_Not_found
(** Raised when attempting to access a node that doesn't exist in the DAG *)

exception DagNode_Already_Exists
(** Raised when attempting to exclusively add a node that already exists *)

(** {1 Construction} *)

val init : unit -> 'a t
(** [init ()] creates a new empty DAG *)

val add_node : 'a -> 'a t -> unit
(** [add_node n dag] adds node [n] to [dag]. Does nothing if [n] already exists *)

val add_node_exclusive : 'a -> 'a t -> unit
(** [add_node_exclusive n dag] adds node [n] to [dag].
    @raise DagNode_Already_Exists if [n] already exists *)

val add_edge : 'a -> 'a -> 'a t -> unit
(** [add_edge a b dag] adds a directed edge from [a] to [b].
    [a] becomes a parent of [b], and [b] becomes a child of [a].
    Creates nodes [a] and [b] if they don't exist *)

val add_edges : ('a * 'a) list -> 'a t -> unit
(** [add_edges edges dag] adds multiple edges to [dag].
    Equivalent to [List.iter (fun (a,b) -> add_edge a b dag) edges] *)

val add_edges_connected : 'a list -> 'a t -> unit
(** [add_edges_connected [n1; n2; n3; ...]] creates a chain of edges:
    n1 -> n2 -> n3 -> ... *)

val add_children_edges : 'a -> 'a list -> 'a t -> unit
(** [add_children_edges parent children dag] adds edges from [parent]
    to each element in [children] *)

val del_edge : 'a -> 'a -> 'a t -> unit
(** [del_edge a b dag] removes the edge from [a] to [b] if it exists *)

(** {1 Queries} *)

val length : 'a t -> int
(** [length dag] returns the number of nodes in [dag] *)

val exists_node : 'a -> 'a t -> bool
(** [exists_node n dag] returns [true] if node [n] exists in [dag] *)

val has_edge : 'a -> 'a -> 'a t -> bool
(** [has_edge a b dag] returns [true] if there is an edge from [a] to [b] *)

val get_node : 'a t -> 'a -> 'a dagnode
(** [get_node dag n] returns the node structure for [n].
    @raise DagNode_Not_found if [n] doesn't exist *)

val get_nodes : 'a t -> 'a list
(** [get_nodes dag] returns all nodes in [dag] *)

val get_leaves : 'a t -> 'a list
(** [get_leaves dag] returns all nodes with no children *)

val get_roots : 'a t -> 'a list
(** [get_roots dag] returns all nodes with no parents *)

val get_children : 'a t -> 'a -> 'a list
(** [get_children dag n] returns the immediate children of [n].
    @raise DagNode_Not_found if [n] doesn't exist *)

val get_parents : 'a t -> 'a -> 'a list
(** [get_parents dag n] returns the immediate parents of [n].
    @raise DagNode_Not_found if [n] doesn't exist *)

val get_children_full : 'a t -> 'a -> 'a list
(** [get_children_full dag n] returns all descendants of [n] (transitive closure).
    @raise DagNode_Not_found if [n] doesn't exist *)

val is_children : 'a t -> 'a -> 'a -> bool
(** [is_children dag a b] returns [true] if [b] is an immediate child of [a] *)

val is_children_full : 'a t -> 'a -> 'a -> bool
(** [is_children_full dag a b] returns [true] if [b] is a descendant of [a]
    (checks transitive closure) *)

(** {1 Operations} *)

val copy : 'a t -> 'a t
(** [copy dag] creates a deep copy of [dag] *)

val subset : 'a t -> 'a list -> 'a t
(** [subset dag roots] creates a new DAG containing only the subgraph
    reachable from [roots] *)

val merge : 'a t -> 'a t -> 'a list
(** [merge dest src] merges [src] into [dest] and returns a list of
    duplicate nodes (nodes that existed in both DAGs) *)

val transitive_reduction : 'a t -> 'a t
(** [transitive_reduction dag] returns a new DAG with redundant edges removed.
    An edge (a,c) is redundant if there exists a path a -> b -> c.
    WARNING: O(v³) complexity - use with care *)

(** {1 Debugging} *)

val dump : ('a -> string) -> 'a t -> unit
(** [dump to_string dag] prints the DAG structure to stdout for debugging *)

val to_dot : ('a -> string) -> string -> bool -> 'a t -> string
(** [to_dot to_string name from_leaf dag] generates a GraphViz DOT representation.
    - [to_string]: function to convert nodes to strings
    - [name]: name of the graph
    - [from_leaf]: if [true], edges point from leaves to roots *)
