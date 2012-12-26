open Types
open Printf

let getGeneralConfig () =
    { conf_verbosity = Report 
    ; conf_withopt = true
    }

let verbose generalConf lvl fmt =
    if lvl <= generalConf.conf_verbosity
        then printf fmt
        else ifprintf stdout fmt

type execState = Success of string * string | Failure of string

let run_with_outputs args =
    let (r1,w1) = Unix.pipe () in
    let (r2,w2) = Unix.pipe () in
    let outbuf = Buffer.create 1024 in
    let errbuf = Buffer.create 1024 in
    let argv = Array.of_list args in
    let pid = Unix.create_process argv.(0) argv Unix.stdin w1 w2 in
    List.iter Unix.close [w1;w2];
    let readfds = ref [r1;r2] in
    let b = String.create 1024 in
    while !readfds <> []
    do
        let (rs, _, _) = Unix.select !readfds [] [] 2.0 in
        if List.mem r1 rs then (
            let nb = Unix.read r1 b 0 1024 in
            if nb > 0
                then Buffer.add_substring outbuf b 0 nb
                else readfds := List.filter (fun x -> x <> r1) !readfds
        );
        if List.mem r2 rs then (
            let nb = Unix.read r2 b 0 1024 in
            if nb > 0
                then Buffer.add_substring errbuf b 0 nb
                else readfds := List.filter (fun x -> x <> r2) !readfds
        );
    done;

    let (_, pstat) = Unix.waitpid [] pid in
    match pstat with
    | Unix.WEXITED 0 -> Success (Buffer.contents outbuf, Buffer.contents errbuf)
    | Unix.WEXITED n -> Failure (Buffer.contents errbuf)
    | _              -> Failure ""

