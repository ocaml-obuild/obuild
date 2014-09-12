open Printf
open Ext.Fugue

type direction = FromChildren | FromParent

(* this is a simple task dependency 'scheduler' *)
(* TODO Set *)
type 'a t = {
  dag               : 'a Dag.t;
  nb_steps          : int;
  steps_done          : ('a, unit) Hashtbl.t;
  direction         : direction;
  mutable current_step   : int;
  mutable next_tasks : 'a list;
}

(* init a new taskdep from a dag *)
let init_with dag direction nodes = {
  dag       = dag;
  nb_steps   = Dag.length dag;
  current_step   = 1;
  direction = direction;
  steps_done  = Hashtbl.create 16;
  next_tasks = nodes;
}

let init ?(direction=FromChildren) dag =
  init_with dag direction (if direction = FromChildren then Dag.getLeaves dag else Dag.getRoots dag)

let next_index taskdep =
  let c = taskdep.current_step in
  taskdep.current_step <- taskdep.current_step + 1;
  c

(* get next task from the task dependency, and removes it from the next list *)
let get_next taskdep =
  let nexts = taskdep.next_tasks in
  match nexts with
  | []       -> None
  | task::xs ->
    taskdep.next_tasks <- xs;
    Some (next_index taskdep, task)

let mark_done taskdep step =
  Hashtbl.add taskdep.steps_done step ();
  (* check if any parents is now free to complete *)
  let parents = if taskdep.direction = FromChildren
    then Dag.getParents taskdep.dag step
    else Dag.getChildren taskdep.dag step in
  List.iter (fun parent ->
      let children =
        if taskdep.direction = FromChildren
        then Dag.getChildren taskdep.dag parent
        else Dag.getParents taskdep.dag parent
      in
      let allDone  = List.for_all (fun child -> Hashtbl.mem taskdep.steps_done child) children in
      if allDone && not (List.mem parent taskdep.next_tasks) then
        taskdep.next_tasks <- taskdep.next_tasks @ [parent]
    ) parents

let is_complete taskdep =
  Hashtbl.length taskdep.steps_done = taskdep.nb_steps

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
  printf "tasks steps done: [%s]\n" (String.concat "," (List.map a_to_string (hashtbl_keys taskdep.steps_done)));
  printf "tasks next: [%s]\n" (String.concat "," (List.map a_to_string taskdep.next_tasks))
