(* simple bi-directional DAG implementation using shallow link*)
open Printf
open Ext.Compat
    
(* represent a node that point shallowly to children and parents *)
type 'a dagnode =
    { mutable parents  : 'a list
    ; mutable children : 'a list
    }

(* TODO add a 'a <-> int table, so that indexing can be done on int instead and
   that lists can be replaced by set *)
type 'a t =
    { nodes : ('a, 'a dagnode) Hashtbl.t
    }

let init () = { nodes = Hashtbl.create 16 }

let length dag = Hashtbl.length dag.nodes

(* Add an directed edge from a to b.
 *
 * 'a' is the parent of 'b'
 * 'b' is the child of 'a'
 *)
let addEdge a b dag =
    let maNode = try Some (Hashtbl.find dag.nodes a) with Not_found -> None in
    let mbNode = try Some (Hashtbl.find dag.nodes b) with Not_found -> None in
    (match (maNode, mbNode) with
    | None, None       ->
        Hashtbl.add dag.nodes a { parents = []; children = [b] };
        Hashtbl.add dag.nodes b { parents = [a]; children = [] }
    | Some aNode, None ->
        if not (List.mem b aNode.children) then aNode.children <- b :: aNode.children;
        Hashtbl.add dag.nodes b { parents = [a]; children = [] }
    | None, Some bNode ->
        if not (List.mem a bNode.children) then bNode.parents <- a :: bNode.parents;
        Hashtbl.add dag.nodes a { parents = []; children = [b] }
    | Some aNode, Some bNode ->
        if not (List.mem b aNode.children) then aNode.children <- b :: aNode.children;
        if not (List.mem a bNode.children) then bNode.parents <- a :: bNode.parents
    );
    ()

exception DagNode_Not_found
exception DagNode_Already_Exists

let addNode a dag =
    try let _ = Hashtbl.find dag.nodes a in ()
    with Not_found -> Hashtbl.add dag.nodes a { parents = []; children = [] }

let addNode_exclusive a dag =
    try let _ = Hashtbl.find dag.nodes a in raise DagNode_Already_Exists
    with Not_found -> Hashtbl.add dag.nodes a { parents = []; children = [] }

(* has edge from a to b *)
let hasEdge a b dag =
    let maNode = try Some (Hashtbl.find dag.nodes a) with Not_found -> None in
    let mbNode = try Some (Hashtbl.find dag.nodes b) with Not_found -> None in
    match (maNode, mbNode) with
    | Some aNode, Some bNode -> List.mem b aNode.children && List.mem a bNode.parents
    | _                      -> false

let delEdge a b dag =
    let maNode = try Some (Hashtbl.find dag.nodes a) with Not_found -> None in
    let mbNode = try Some (Hashtbl.find dag.nodes b) with Not_found -> None in
    (match (maNode, mbNode) with
    | Some aNode, Some bNode ->
        aNode.children <- List.filter (fun x -> x <> b) aNode.children;
        bNode.parents  <- List.filter (fun x -> x <> a) bNode.parents
    | _ -> ()
    )

let addEdges l dag =
    List.iter (fun (n1, n2) -> addEdge n1 n2 dag) l

(*  add edges connected to each other in a list
 *  n1 -> n2 -> n3 -> ... -> nn
 *)
let addEdgesConnected l dag =
    let rec loop parent nodes =
        match nodes with
        | []    -> ()
        | n::ns -> addEdge parent n dag; loop n ns
        in
    match l with
    | []    -> ()
    | x::[] -> addNode x dag
    | x::l  -> loop x l


(*  add children edges with p the parent
 *  p -> l[1], p -> l[2], ..., p -> l[n]
 *)
let addChildrenEdges p l dag =
    List.iter (fun x -> addEdge p x dag) l

let existsNode a dag = Hashtbl.mem dag.nodes a

let getLeaves dag =
    Hashtbl.fold (fun k v acc -> if v.children = [] then k::acc else acc) dag.nodes []

let getRoots dag =
    Hashtbl.fold (fun k v acc -> if v.parents = [] then k::acc else acc) dag.nodes []

let getNode dag a = try Hashtbl.find dag.nodes a
                    with Not_found -> raise DagNode_Not_found
let getNodes dag = Hashtbl.fold (fun k _ acc -> k :: acc) dag.nodes []

let getChildren dag a = (getNode dag a).children

let getParents dag a = (getNode dag a).parents

let rec getChildren_full dag a = 
    let children = getChildren dag a in
    children @ List.concat (List.map (getChildren_full dag) children)

let isChildren dag a b = List.mem b (getChildren dag a)

let rec isChildren_full dag a b =
    let children = getChildren dag a in
    (* either it's present here, or in one of the kiddy *)
    List.mem b children ||
    List.fold_left (fun acc child ->
        acc || isChildren_full dag child b
    ) false children

let subset dag roots =
    let subdag = init () in
    let rec loop node =
        addNode node subdag;
        let children = getChildren dag node in
        List.iter (fun child -> addEdge node child subdag; loop child) children
        in
    List.iter (fun root -> loop root) roots;
    subdag

let copy dag =
    let nodes = Hashtbl.fold (fun k _ acc -> k :: acc) dag.nodes [] in
    let dag2 = init () in
    let copy_node node =
        addNode node dag2;
        let children = getChildren dag node in
        addChildrenEdges node children dag2
        in
    List.iter (fun node -> copy_node node) nodes;
    dag2

let merge dest src =
  let nodes = Hashtbl.fold (fun k _ acc -> k :: acc) src.nodes [] in
  let dups = ref [] in
  List.iter (fun node -> if existsNode node dest then dups := node :: !dups) nodes;
  let copy_node node =
    addNode node dest;
    let children = getChildren src node in
    addChildrenEdges node children dest
  in
  List.iter (fun node -> copy_node node) nodes;
  !dups

(* o(v^3) use with care *)
let transitive_reduction dag =
    let reducedDag = copy dag in
    (* this is sub optimal, as we re-lookup nodes everytimes in hasEdge AND delEdge.
     * would go away automatically when having the lookup dict with sets. *)
    let nodes = Hashtbl.fold (fun k _ acc -> k :: acc) dag.nodes [] in
    List.iter (fun x ->
        List.iter (fun y ->
            List.iter (fun z ->
                if hasEdge x y dag && hasEdge y z dag
                    then delEdge x z reducedDag
                    else ()
            ) nodes
        ) nodes
    ) nodes;
    reducedDag

(* this is for debugging the DAG.
 * dump the dag links and node in a textual format *)
let dump a_to_string dag =
    let all = getNodes dag in
    List.iter (fun n ->
        printf "%s:\n" (a_to_string n);
        printf "  | parents  = %s\n" (String.concat ", " (List.map a_to_string (getParents dag n)));
        printf "  | children = %s\n" (String.concat ", " (List.map a_to_string (getChildren dag n)))
    ) all

(* it's useful to be able to visualize the DAG with the excellent dot
 *)
let toDot a_to_string name fromLeaf dag =
    let buf = Buffer.create 1024 in
    let nodes = getNodes dag in
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
        ) ((if fromLeaf then getParents else getChildren) dag n)
    ) nodes;
    
    append "}\n";
    Buffer.contents buf
