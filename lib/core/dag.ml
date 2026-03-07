(* Optimized bi-directional DAG implementation using sets and int indexing *)
open Printf
open Compat

(* IntSet for efficient membership and removal operations *)
module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)

(* Internal representation: nodes are mapped to integer IDs,
   and parent/child relationships use IntSet for O(log n) operations *)
type 'a dagnode =
    { mutable parents  : IntSet.t
    ; mutable children : IntSet.t
    }

type 'a t =
    { nodes : (int, 'a dagnode) Hashtbl.t  (* ID -> node structure *)
    ; node_to_id : ('a, int) Hashtbl.t      (* node -> ID mapping *)
    ; id_to_node : (int, 'a) Hashtbl.t      (* ID -> node mapping *)
    ; mutable next_id : int                 (* counter for new IDs *)
    }

let init () =
    { nodes = Hashtbl.create 16
    ; node_to_id = Hashtbl.create 16
    ; id_to_node = Hashtbl.create 16
    ; next_id = 0
    }

(* Get or create ID for a node *)
let get_node_id dag node =
    match SafeHashtbl.find_opt dag.node_to_id node with
    | Some id -> id
    | None ->
        let id = dag.next_id in
        dag.next_id <- dag.next_id + 1;
        Hashtbl.add dag.node_to_id node id;
        Hashtbl.add dag.id_to_node id node;
        id

let length dag = Hashtbl.length dag.nodes

(* Add an directed edge from a to b.
 *
 * 'a' is the parent of 'b'
 * 'b' is the child of 'a'
 *)
let add_edge a b dag =
    let aid = get_node_id dag a in
    let bid = get_node_id dag b in
    let maNode = SafeHashtbl.find_opt dag.nodes aid in
    let mbNode = SafeHashtbl.find_opt dag.nodes bid in
    (match (maNode, mbNode) with
    | None, None       ->
        Hashtbl.add dag.nodes aid { parents = IntSet.empty; children = IntSet.singleton bid };
        Hashtbl.add dag.nodes bid { parents = IntSet.singleton aid; children = IntSet.empty }
    | Some aNode, None ->
        aNode.children <- IntSet.add bid aNode.children;
        Hashtbl.add dag.nodes bid { parents = IntSet.singleton aid; children = IntSet.empty }
    | None, Some bNode ->
        bNode.parents <- IntSet.add aid bNode.parents;
        Hashtbl.add dag.nodes aid { parents = IntSet.empty; children = IntSet.singleton bid }
    | Some aNode, Some bNode ->
        aNode.children <- IntSet.add bid aNode.children;
        bNode.parents <- IntSet.add aid bNode.parents
    );
    ()

exception DagNodeNotFound
exception DagNodeAlreadyExists

let add_node a dag =
    let aid = get_node_id dag a in
    if not (Hashtbl.mem dag.nodes aid) then
        Hashtbl.add dag.nodes aid { parents = IntSet.empty; children = IntSet.empty }

let add_node_exclusive a dag =
    let aid = get_node_id dag a in
    if Hashtbl.mem dag.nodes aid then
        raise DagNodeAlreadyExists
    else
        Hashtbl.add dag.nodes aid { parents = IntSet.empty; children = IntSet.empty }

(* has edge from a to b *)
let has_edge a b dag =
    match SafeHashtbl.find_opt dag.node_to_id a, SafeHashtbl.find_opt dag.node_to_id b with
    | Some aid, Some bid ->
        (match SafeHashtbl.find_opt dag.nodes aid, SafeHashtbl.find_opt dag.nodes bid with
        | Some aNode, Some bNode -> IntSet.mem bid aNode.children && IntSet.mem aid bNode.parents
        | _ -> false)
    | _ -> false

let del_edge a b dag =
    match SafeHashtbl.find_opt dag.node_to_id a, SafeHashtbl.find_opt dag.node_to_id b with
    | Some aid, Some bid ->
        (match SafeHashtbl.find_opt dag.nodes aid, SafeHashtbl.find_opt dag.nodes bid with
        | Some aNode, Some bNode ->
            aNode.children <- IntSet.remove bid aNode.children;
            bNode.parents  <- IntSet.remove aid bNode.parents
        | _ -> ())
    | _ -> ()

let add_edges l dag =
    List.iter (fun (n1, n2) -> add_edge n1 n2 dag) l

(*  add edges connected to each other in a list
 *  n1 -> n2 -> n3 -> ... -> nn
 *)
let add_edges_connected l dag =
    let rec loop parent nodes =
        match nodes with
        | []    -> ()
        | n::ns -> add_edge parent n dag; loop n ns
        in
    match l with
    | []    -> ()
    | x::[] -> add_node x dag
    | x::l  -> loop x l


(*  add children edges with p the parent
 *  p -> l[1], p -> l[2], ..., p -> l[n]
 *)
let add_children_edges p l dag =
    List.iter (fun x -> add_edge p x dag) l

let exists_node a dag = Hashtbl.mem dag.node_to_id a

let get_leaves dag =
    Hashtbl.fold (fun id v acc ->
        if IntSet.is_empty v.children then
            match SafeHashtbl.find_opt dag.id_to_node id with
            | Some node -> node :: acc
            | None -> acc  (* Should not happen - ID exists in nodes *)
        else acc
    ) dag.nodes []

let get_roots dag =
    Hashtbl.fold (fun id v acc ->
        if IntSet.is_empty v.parents then
            match SafeHashtbl.find_opt dag.id_to_node id with
            | Some node -> node :: acc
            | None -> acc  (* Should not happen - ID exists in nodes *)
        else acc
    ) dag.nodes []

let get_node dag a =
    match SafeHashtbl.find_opt dag.node_to_id a with
    | Some aid ->
        (match SafeHashtbl.find_opt dag.nodes aid with
        | Some node -> node
        | None -> raise DagNodeNotFound)
    | None -> raise DagNodeNotFound

let get_nodes dag =
    Hashtbl.fold (fun id _ acc ->
        match SafeHashtbl.find_opt dag.id_to_node id with
        | Some node -> node :: acc
        | None -> acc  (* Should not happen - ID exists in nodes *)
    ) dag.nodes []

let get_children dag a =
    let node = get_node dag a in
    IntSet.fold (fun id acc ->
        match SafeHashtbl.find_opt dag.id_to_node id with
        | Some n -> n :: acc
        | None -> acc  (* Should not happen - ID in children set *)
    ) node.children []

let get_parents dag a =
    let node = get_node dag a in
    IntSet.fold (fun id acc ->
        match SafeHashtbl.find_opt dag.id_to_node id with
        | Some n -> n :: acc
        | None -> acc  (* Should not happen - ID in parents set *)
    ) node.parents []

let get_children_full dag a =
    let visited = Hashtbl.create 16 in
    let result = ref [] in
    let queue = Queue.create () in
    List.iter (fun c -> Queue.push c queue) (get_children dag a);
    while not (Queue.is_empty queue) do
      let node = Queue.pop queue in
      if not (Hashtbl.mem visited node) then begin
        Hashtbl.replace visited node ();
        result := node :: !result;
        List.iter (fun c -> Queue.push c queue) (get_children dag node)
      end
    done;
    List.rev !result

let is_children dag a b = List.mem b (get_children dag a)

let is_children_full dag a b =
    let visited = Hashtbl.create 16 in
    let queue = Queue.create () in
    List.iter (fun c -> Queue.push c queue) (get_children dag a);
    let found = ref false in
    while not (Queue.is_empty queue) && not !found do
        let node = Queue.pop queue in
        if node = b then found := true
        else if not (Hashtbl.mem visited node) then begin
            Hashtbl.replace visited node ();
            List.iter (fun c -> Queue.push c queue) (get_children dag node)
        end
    done;
    !found

let subset dag roots =
    let subdag = init () in
    let rec loop node =
        add_node node subdag;
        let children = get_children dag node in
        List.iter (fun child -> add_edge node child subdag; loop child) children
        in
    List.iter (fun root -> loop root) roots;
    subdag

let copy dag =
    let nodes = get_nodes dag in
    let dag2 = init () in
    let copy_node node =
        add_node node dag2;
        let children = get_children dag node in
        add_children_edges node children dag2
        in
    List.iter (fun node -> copy_node node) nodes;
    dag2

let merge dest src =
  let nodes = get_nodes src in
  let dups = ref [] in
  List.iter (fun node -> if exists_node node dest then dups := node :: !dups) nodes;
  let copy_node node =
    add_node node dest;
    let children = get_children src node in
    add_children_edges node children dest
  in
  List.iter (fun node -> copy_node node) nodes;
  !dups

(* O(v^3) use with care.
 * Precomputes an adjacency matrix indexed by internal integer IDs so that the
 * inner loop uses O(1) array reads instead of the 4-hashtable-lookup has_edge. *)
let transitive_reduction dag =
    let reducedDag = copy dag in
    let n = dag.next_id in
    if n = 0 then reducedDag
    else begin
        (* Build adjacency matrix from the original dag *)
        let adj = Array.make_matrix n n false in
        Hashtbl.iter (fun id node ->
            IntSet.iter (fun cid -> adj.(id).(cid) <- true) node.children
        ) dag.nodes;
        (* Collect node IDs once to avoid repeated Hashtbl.iter *)
        let ids = Hashtbl.fold (fun id _ acc -> id :: acc) dag.nodes [] in
        (* Triple loop: if x→y and y→z exist in original, remove x→z from reduced *)
        List.iter (fun xid ->
            List.iter (fun yid ->
                if adj.(xid).(yid) then
                    List.iter (fun zid ->
                        if adj.(yid).(zid) then begin
                            let xval = Hashtbl.find dag.id_to_node xid in
                            let zval = Hashtbl.find dag.id_to_node zid in
                            del_edge xval zval reducedDag
                        end
                    ) ids
            ) ids
        ) ids;
        reducedDag
    end

(* this is for debugging the DAG.
 * dump the dag links and node in a textual format *)
let dump a_to_string dag =
    let all = get_nodes dag in
    List.iter (fun n ->
        printf "%s:\n" (a_to_string n);
        printf "  | parents  = %s\n" (String.concat ", " (List.map a_to_string (get_parents dag n)));
        printf "  | children = %s\n" (String.concat ", " (List.map a_to_string (get_children dag n)))
    ) all

(* it's useful to be able to visualize the DAG with the excellent dot
 *)
let to_dot a_to_string name fromLeaf dag =
    let buf = Buffer.create 1024 in
    let nodes = get_nodes dag in
    let dotIndex = Hashtbl.create (List.length nodes) in
    let append = Buffer.add_string buf in
    let sanitizeName = bytes_of_string name in
    for i = 0 to String.length name - 1
    do
      if (bytes_get sanitizeName i) = '-'
      then bytes_set sanitizeName i '_'
    done;

    append ("digraph " ^ (bytes_to_string sanitizeName) ^ " {\n");

    let list_iteri f list =
        let rec loop i l =
            match l with
            | []    -> ()
            | x::xs -> f i x; loop (i+1) xs
            in
        loop 1 list
        in

    list_iteri (fun i n ->
        Hashtbl.add dotIndex n i;
        append (sprintf "  %d [label = \"%s\"];\n" i (a_to_string n));
    ) nodes;

    List.iter (fun n ->
        let i = Hashtbl.find dotIndex n in
        List.iter (fun child ->
            let ci = Hashtbl.find dotIndex child in
            append (sprintf "  %d -> %d;\n" i ci)
        ) ((if fromLeaf then get_parents else get_children) dag n)
    ) nodes;
    
    append "}\n";
    Buffer.contents buf
