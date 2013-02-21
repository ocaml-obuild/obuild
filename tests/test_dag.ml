open Obuild

let err = ref 0

(* simple dag: a -> b -> c *)
let d1 =
    let d = Dag.init () in
    Dag.addEdge "A" "B" d;
    Dag.addEdge "B" "C" d;
    d

(* DAG with a fork
 *
 * A -> B -> C -> D -> E -> F
 *        \> C'-> D'-/
 *)
let d2 =
    let d = Dag.init () in
    Dag.addEdgesConnected ["A";"B";"C";"D";"E";"F"] d;
    Dag.addEdges [ ("B","C'"); ("C'","D'"); ("D'", "E") ] d;
    d

(* DAG
 *      A --------> C
 *       \-> B --/
 *)
let d3 =
    let d = Dag.init () in
    Dag.addEdges [("A","C"); ("A","B"); ("B","C")] d;
    d

(* DAG
 *   A  \     /-> C
 *       -> B
 *   A' /     \-> C'
 *)
let d4 =
    let d = Dag.init () in
    Dag.addEdges [("A","B"); ("A'","B"); ("B","C"); ("B","C'")] d;
    d

let showDeps prefix l = Printf.printf "%s%s\n" prefix (String.concat " -> " l)

let assumeEqF f testname expected got =
    if f expected got
        then (Printf.printf "SUCCESS %s\n" testname)
        else (Printf.printf "FAILED %s\n" testname; showDeps "expected:" (List.concat expected); showDeps "got     :" got; err := !err + 1)

let assumeEq testname expected got =
    if expected = got
        then (Printf.printf "SUCCESS %s\n" testname)
        else (Printf.printf "FAILED %s\n" testname; showDeps "expected:" expected; showDeps "got     :" got; err := !err + 1)

let listEq a b =
    let rec loopElem l r =
        match l with
        | [] -> (true, r)
        | _  -> match r with
                   | []    -> (false, r)
                   | e::es ->
                           if List.mem e l
                                then loopElem (List.filter (fun z -> z <> e) l) es
                                else (false, r)
        in
    let rec loopGroup l r =
        match l with
        | []    -> if r = [] then true else false
        | g::gs ->
            let (e,r2) = loopElem g r in
            if e = true
                then loopGroup gs r2
                else false
        in
    loopGroup a b

let () =
    let l1 = Taskdep.linearize d1 Taskdep.FromParent ["A"] in
    let l2 = Taskdep.linearize d2 Taskdep.FromParent ["A"] in
    let l2' = Taskdep.linearize d2 Taskdep.FromParent ["C'"] in
    let l3 = Taskdep.linearize d3 Taskdep.FromParent ["A"] in
    let l3' = Taskdep.linearize (Dag.transitive_reduction d3) Taskdep.FromParent ["A"] in
    let l4 = Taskdep.linearize d4 Taskdep.FromParent ["A"; "A'"] in

    assumeEq "linearization A->B->C" [ "A"; "B"; "C" ] l1;
    assumeEq "linearization A->B->(C,C')->(D,D')->E->F" ["A";"B";"C";"D";"C'";"D'";"E";"F"] l2;
    assumeEq "linearization C'->D'->E->F" ["C'";"D'";"E";"F"] l2';
    assumeEq "linearization A->(B->C)" ["A";"B";"C"] l3;
    assumeEq "linearization A->(B->C)" ["A";"B";"C"] l3';
    assumeEqF listEq "linearization (A,A')->B->(C,C')" [["A";"A'"];["B"];["C";"C'"]] l4;

    if !err > 1
        then exit 1
        else exit 0
