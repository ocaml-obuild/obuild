let iter f dag =
    let tdep = Taskdep.init dag in
    while not (Taskdep.is_complete tdep) do
        match Taskdep.get_next tdep with
        | None          -> failwith "taskdep dag next didn't work"
        | Some (_,task) -> f task; Taskdep.mark_done tdep task
    done

let iteri f dag =
    let tdep = Taskdep.init dag in
    while not (Taskdep.is_complete tdep) do
        match Taskdep.get_next tdep with
        | None            -> failwith "taskdep dag next didn't work"
        | Some (idx,task) -> f idx task; Taskdep.mark_done tdep task
    done

let linearize dag =
    let tdep = Taskdep.init dag in
    let rec loop () =
        if Taskdep.is_complete tdep
            then []
            else (
                match Taskdep.get_next tdep with
                | None            -> failwith "taskdep dag next didn't work"
                | Some (_,task) -> Taskdep.mark_done tdep task; task :: loop ()
            )
        in
    loop ()

