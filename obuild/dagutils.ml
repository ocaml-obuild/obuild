
let iter f dag =
    let tdep = Taskdep.init dag in
    while not (Taskdep.isComplete tdep) do
        match Taskdep.getnext tdep with
        | None          -> failwith "taskdep dag next didn't work"
        | Some (_,task) -> f task; Taskdep.markDone tdep task
    done

let iteri f dag =
    let tdep = Taskdep.init dag in
    while not (Taskdep.isComplete tdep) do
        match Taskdep.getnext tdep with
        | None            -> failwith "taskdep dag next didn't work"
        | Some (idx,task) -> f idx task; Taskdep.markDone tdep task
    done

let linearize dag =
    let tdep = Taskdep.init dag in
    let rec loop () =
        if Taskdep.isComplete tdep
            then []
            else (
                match Taskdep.getnext tdep with
                | None            -> failwith "taskdep dag next didn't work"
                | Some (idx,task) -> Taskdep.markDone tdep task; task :: loop ()
            )
        in
    loop ()

