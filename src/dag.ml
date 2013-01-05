(* simple bi-directional DAG implementation using shallow link*)
open Printf
open Ext

(* represent a node that point shallowly to children and parents *)
type 'a dagnode = { mutable parents  : 'a list
                  ; mutable children : 'a list
                  }

(* TODO add a 'a <-> int table, so that indexing can be done on int *)
type 'a t = { nodes : ('a, 'a dagnode) Hashtbl.t }


let init () = { nodes = Hashtbl.create 16 }

let length dag = Hashtbl.length dag.nodes

(* add an edge from a to b *)
let addEdge a b dag =
    let maNode = try Some (Hashtbl.find dag.nodes a) with Not_found -> None in
    let mbNode = try Some (Hashtbl.find dag.nodes b) with Not_found -> None in
    (match (maNode, mbNode) with
    | None, None       -> Hashtbl.add dag.nodes a { parents = []; children = [b] };
                          Hashtbl.add dag.nodes b { parents = [a]; children = [] }
    | Some aNode, None -> aNode.children <- b :: aNode.children;
                          Hashtbl.add dag.nodes b { parents = [a]; children = [] }
    | None, Some bNode -> bNode.parents <- a :: bNode.parents;
                          Hashtbl.add dag.nodes a { parents = []; children = [b] }
    | Some aNode, Some bNode ->
                          aNode.children <- b :: aNode.children;
                          bNode.parents <- a :: bNode.parents
    );
    ()

exception DagNode_Not_found
exception DagNode_Already_Exists

let addNode a dag =
    try let _ = Hashtbl.find dag.nodes a in raise DagNode_Already_Exists
    with Not_found -> Hashtbl.add dag.nodes a { parents = []; children = [] }

let existsNode a dag =
    try let _ = Hashtbl.find dag.nodes a in true
    with Not_found -> false

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

(* this is a simple task dependency 'scheduler' *)
type 'a taskdep =
    { taskdep_dag               : 'a t
    ; taskdep_nbSteps           : int
    ; taskdep_done              : ('a, unit) Hashtbl.t
    ; mutable taskdep_curStep   : int
    ; mutable taskdep_nextTasks : 'a list
    }

let taskdep_init dag =
    { taskdep_dag       = dag
    ; taskdep_nbSteps   = length dag
    ; taskdep_curStep   = 1
    ; taskdep_done      = Hashtbl.create 16
    ; taskdep_nextTasks = getLeaves dag
    }

let taskdep_getnext_index taskdep =
    let c = taskdep.taskdep_curStep in
    taskdep.taskdep_curStep <- taskdep.taskdep_curStep + 1;
    c

(* get next task from the task dependency, and removes it from the next list *)
let taskdep_getnext taskdep =
    let nexts = taskdep.taskdep_nextTasks in
    match nexts with
    | []       -> None
    | task::xs ->
        taskdep.taskdep_nextTasks <- xs;
        Some (taskdep_getnext_index taskdep, task)

(* just like taskdep_getnext except we let a user function choose
 * the next elements between all the possible tasks
 *)
let taskdep_getnext_choice f taskdep =
    let nexts = taskdep.taskdep_nextTasks in
    match nexts with
    | [] -> None
    | l  ->
        let task = f l in
        taskdep.taskdep_nextTasks <- list_remove task l;
        Some (taskdep_getnext_index taskdep, task)
    
let taskdep_markDone taskdep step =
    Hashtbl.add taskdep.taskdep_done step ();
    (* check if any parents is now free to complete *)
    let parents = getParents taskdep.taskdep_dag step in
    List.iter (fun parent ->
        let children = getChildren taskdep.taskdep_dag parent in
        let allDone  = List.for_all (fun child -> Hashtbl.mem taskdep.taskdep_done child) children in
        if allDone then
            taskdep.taskdep_nextTasks <- taskdep.taskdep_nextTasks @ [parent]
    ) parents

let taskdep_isComplete taskdep =
    Hashtbl.length taskdep.taskdep_done = taskdep.taskdep_nbSteps

let taskdep_dump a_to_string taskdep =
    printf "tasks done: [%s]\n" (String.concat "," (List.map a_to_string (hashtbl_keys taskdep.taskdep_done)));
    printf "tasks next: [%s]\n" (String.concat "," (List.map a_to_string taskdep.taskdep_nextTasks))

let dump a_to_string dag =
    let all = getNodes dag in
    List.iter (fun n ->
        printf "%s:\n" (a_to_string n);
        printf "  | parents  = %s\n" (String.concat ", " (List.map a_to_string (getParents dag n)));
        printf "  | children = %s\n" (String.concat ", " (List.map a_to_string (getChildren dag n)))
    ) all

(* this is for debugging the DAG.
 * it's useful to be able to visualize the DAG with the excellent dot
 **)
let toDot a_to_string name fromLeaf dag =
    let buf = Buffer.create 1024 in
    let nodes = getNodes dag in
    let dotIndex = Hashtbl.create (List.length nodes) in
    let append = Buffer.add_string buf in
    append ("digraph " ^ name ^ " {\n");

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
