open Lib

let err = ref 0

(* simple dag: a -> b -> c *)
let d1 =
  let d = Dag.init () in
  Dag.add_edge "A" "B" d;
  Dag.add_edge "B" "C" d;
  d

(* DAG with a fork
 *
 * A -> B -> C -> D -> E -> F
 *        \> C'-> D'-/
 *)
let d2 =
  let d = Dag.init () in
  Dag.add_edges_connected [ "A"; "B"; "C"; "D"; "E"; "F" ] d;
  Dag.add_edges [ ("B", "C'"); ("C'", "D'"); ("D'", "E") ] d;
  d

(* DAG
 *      A --------> C
 *       \-> B --/
 *)
let d3 =
  let d = Dag.init () in
  Dag.add_edges [ ("A", "C"); ("A", "B"); ("B", "C") ] d;
  d

(* DAG
 *   A  \     /-> C
 *       -> B
 *   A' /     \-> C'
 *)
let d4 =
  let d = Dag.init () in
  Dag.add_edges [ ("A", "B"); ("A'", "B"); ("B", "C"); ("B", "C'") ] d;
  d

let showDeps prefix l = Printf.printf "%s%s\n" prefix (String.concat " -> " l)

let assumeEqF f testname expected got =
  if f expected got then
    Printf.printf "SUCCESS %s\n" testname
  else (
    Printf.printf "FAILED %s\n" testname;
    showDeps "expected:" (List.concat expected);
    showDeps "got     :" got;
    err := !err + 1)

let assumeEq testname expected got =
  if expected = got then
    Printf.printf "SUCCESS %s\n" testname
  else (
    Printf.printf "FAILED %s\n" testname;
    showDeps "expected:" expected;
    showDeps "got     :" got;
    err := !err + 1)

let assumeBool testname expected got =
  if expected = got then
    Printf.printf "SUCCESS %s\n" testname
  else (
    Printf.printf "FAILED %s (expected: %b, got: %b)\n" testname expected got;
    err := !err + 1)

let assumeInt testname expected got =
  if expected = got then
    Printf.printf "SUCCESS %s\n" testname
  else (
    Printf.printf "FAILED %s (expected: %d, got: %d)\n" testname expected got;
    err := !err + 1)

let listEq a b =
  let rec loopElem l r =
    match l with
    | [] -> (true, r)
    | _ -> (
        match r with
        | [] -> (false, r)
        | e :: es ->
            if List.mem e l then
              loopElem (List.filter (fun z -> z <> e) l) es
            else
              (false, r))
  in
  let rec loopGroup l r =
    match l with
    | [] -> if r = [] then true else false
    | g :: gs ->
        let e, r2 = loopElem g r in
        if e = true then
          loopGroup gs r2
        else
          false
  in
  loopGroup a b

let () =
  let l1 = Taskdep.linearize d1 Taskdep.FromParent [ "A" ] in
  let l2 = Taskdep.linearize d2 Taskdep.FromParent [ "A" ] in
  let l2' = Taskdep.linearize d2 Taskdep.FromParent [ "C'" ] in
  let l3 = Taskdep.linearize d3 Taskdep.FromParent [ "A" ] in
  let l3' = Taskdep.linearize (Dag.transitive_reduction d3) Taskdep.FromParent [ "A" ] in
  let l4 = Taskdep.linearize d4 Taskdep.FromParent [ "A"; "A'" ] in

  assumeEq "linearization A->B->C" [ "A"; "B"; "C" ] l1;
  assumeEq "linearization A->B->(C,C')->(D,D')->E->F"
    [ "A"; "B"; "C"; "D"; "C'"; "D'"; "E"; "F" ]
    l2;
  assumeEq "linearization C'->D'->E->F" [ "C'"; "D'"; "E"; "F" ] l2';
  assumeEq "linearization A->(B->C)" [ "A"; "B"; "C" ] l3;
  assumeEq "linearization A->(B->C)" [ "A"; "B"; "C" ] l3';
  assumeEqF listEq "linearization (A,A')->B->(C,C')" [ [ "A"; "A'" ]; [ "B" ]; [ "C"; "C'" ] ] l4;

  (* Test basic DAG operations *)
  let d_basic = Dag.init () in
  Dag.add_node "X" d_basic;
  assumeBool "add_node creates node" true (Dag.exists_node "X" d_basic);
  assumeBool "exists_node returns false for missing" false (Dag.exists_node "Y" d_basic);

  Dag.add_edge "X" "Y" d_basic;
  assumeBool "add_edge creates nodes and edge" true (Dag.has_edge "X" "Y" d_basic);
  assumeBool "has_edge returns false for missing edge" false (Dag.has_edge "Y" "X" d_basic);
  assumeInt "length counts nodes" 2 (Dag.length d_basic);

  (* Test get_children and get_parents *)
  let children_x = Dag.get_children d_basic "X" in
  assumeEq "get_children returns children" ["Y"] children_x;
  let parents_y = Dag.get_parents d_basic "Y" in
  assumeEq "get_parents returns parents" ["X"] parents_y;

  (* Test del_edge *)
  Dag.del_edge "X" "Y" d_basic;
  assumeBool "del_edge removes edge" false (Dag.has_edge "X" "Y" d_basic);

  (* Test get_leaves and get_roots *)
  let d_tree = Dag.init () in
  Dag.add_edges [("root", "a"); ("root", "b"); ("a", "c"); ("a", "d")] d_tree;
  let leaves = List.sort String.compare (Dag.get_leaves d_tree) in
  let roots = List.sort String.compare (Dag.get_roots d_tree) in
  assumeEq "get_leaves returns leaf nodes" ["b"; "c"; "d"] leaves;
  assumeEq "get_roots returns root nodes" ["root"] roots;

  (* Test get_children_full *)
  let all_children = List.sort String.compare (Dag.get_children_full d_tree "root") in
  assumeEq "get_children_full returns all descendants" ["a"; "b"; "c"; "d"] all_children;

  (* Test is_children and is_children_full *)
  assumeBool "is_children detects direct child" true (Dag.is_children d_tree "root" "a");
  assumeBool "is_children returns false for non-child" false (Dag.is_children d_tree "root" "c");
  assumeBool "is_children_full detects descendant" true (Dag.is_children_full d_tree "root" "c");
  assumeBool "is_children_full returns false for non-descendant" false (Dag.is_children_full d_tree "b" "c");

  (* Test copy *)
  let d_copy = Dag.copy d_tree in
  assumeBool "copy creates equivalent DAG" true (Dag.has_edge "root" "a" d_copy);
  assumeInt "copy length matches original" (Dag.length d_tree) (Dag.length d_copy);

  (* Test subset *)
  let d_subset = Dag.subset d_tree ["a"] in
  let subset_nodes = List.sort String.compare (Dag.get_nodes d_subset) in
  assumeEq "subset extracts subgraph" ["a"; "c"; "d"] subset_nodes;

  (* Test merge *)
  let d_merge1 = Dag.init () in
  let d_merge2 = Dag.init () in
  Dag.add_edge "A" "B" d_merge1;
  Dag.add_edge "B" "C" d_merge2;
  let dups = Dag.merge d_merge1 d_merge2 in
  assumeBool "merge combines DAGs" true (Dag.has_edge "B" "C" d_merge1);
  assumeEq "merge detects duplicates" ["B"] (List.sort String.compare dups);

  (* Test add_node_exclusive *)
  let d_excl = Dag.init () in
  Dag.add_node_exclusive "E" d_excl;
  assumeBool "add_node_exclusive adds node" true (Dag.exists_node "E" d_excl);

  (* Test exception handling *)
  let exc_raised =
    try
      Dag.add_node_exclusive "E" d_excl;
      false
    with Dag.DagNode_Already_Exists -> true
  in
  assumeBool "add_node_exclusive raises on duplicate" true exc_raised;

  let not_found_raised =
    try
      let _ = Dag.get_node d_excl "NONEXISTENT" in
      false
    with Dag.DagNode_Not_found -> true
  in
  assumeBool "get_node raises on missing node" true not_found_raised;

  if !err > 0 then
    exit 1
  else
    exit 0
