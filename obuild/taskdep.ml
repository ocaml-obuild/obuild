open Printf
open Ext

type taskdep_direction = FromChildren | FromParent

(* this is a simple task dependency 'scheduler' *)
(* TODO Set *)
type 'a taskdep =
    { dag               : 'a Dag.t
    ; nbSteps           : int
    ; stepDone          : ('a, unit) Hashtbl.t
    ; direction         : taskdep_direction
    ; mutable curStep   : int
    ; mutable nextTasks : 'a list
    }

(* init a new taskdep from a dag *)
let init_with dag direction nodes =
    { dag       = dag
    ; nbSteps   = Dag.length dag
    ; curStep   = 1
    ; direction = direction
    ; stepDone  = Hashtbl.create 16
    ; nextTasks = nodes
    }

let init_with_direction dag direction =
    init_with dag direction (if direction = FromChildren then Dag.getLeaves dag else Dag.getRoots dag)

let init dag = init_with_direction dag FromChildren

let getnext_index taskdep =
    let c = taskdep.curStep in
    taskdep.curStep <- taskdep.curStep + 1;
    c

(* get next task from the task dependency, and removes it from the next list *)
let getnext taskdep =
    let nexts = taskdep.nextTasks in
    match nexts with
    | []       -> None
    | task::xs ->
        taskdep.nextTasks <- xs;
        Some (getnext_index taskdep, task)

(* just like getnext except we let a user function choose
 * the next elements between all the possible tasks
 *)
let getnext_choice f taskdep =
    let nexts = taskdep.nextTasks in
    match nexts with
    | [] -> None
    | l  ->
        let task = f l in
        taskdep.nextTasks <- list_remove task l;
        Some (getnext_index taskdep, task)
    
let markDone taskdep step =
    Hashtbl.add taskdep.stepDone step ();
    (* check if any parents is now free to complete *)
    let parents = if taskdep.direction = FromChildren then Dag.getParents taskdep.dag step else Dag.getChildren taskdep.dag step in
    List.iter (fun parent ->
        let children =
            if taskdep.direction = FromChildren
                then Dag.getChildren taskdep.dag parent
                else Dag.getParents taskdep.dag parent
            in
        let allDone  = List.for_all (fun child -> Hashtbl.mem taskdep.stepDone child) children in
        if allDone && not (List.mem parent taskdep.nextTasks) then
            taskdep.nextTasks <- taskdep.nextTasks @ [parent]
    ) parents

let isComplete taskdep =
    Hashtbl.length taskdep.stepDone = taskdep.nbSteps

let linearize dag direction nodes =
    let l = ref [] in
    let visited = Hashtbl.create 16 in
    let rec visit n =
        if not (Hashtbl.mem visited n) then (
            Hashtbl.add visited n ();
            List.iter visit ((if direction = FromParent then Dag.getChildren else Dag.getParents) dag n);
            l := n :: !l;
        )
        in
    List.iter visit nodes;
    !l

let dump a_to_string taskdep =
    printf "tasks stepDone: [%s]\n" (String.concat "," (List.map a_to_string (hashtbl_keys taskdep.stepDone)));
    printf "tasks next: [%s]\n" (String.concat "," (List.map a_to_string taskdep.nextTasks))
