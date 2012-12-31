open Types
open Printf

let getGeneralConfig () =
    { conf_verbosity = Report 
    ; conf_withopt   = true
    ; conf_setup     = []
    }

let verbose generalConf lvl fmt =
    if lvl <= generalConf.conf_verbosity
        then printf fmt
        else ifprintf stdout fmt

type execState = Success of string * string | Failure of string

type exe_state = { outbuf : Buffer.t
                 ; errbuf : Buffer.t
                 ; fd_out : Unix.file_descr
                 ; fd_err : Unix.file_descr
                 ; pid    : int
                 }

let spawn args =
    let (r1,w1) = Unix.pipe () in
    let (r2,w2) = Unix.pipe () in
    let argv = Array.of_list args in
    let pid = Unix.create_process argv.(0) argv Unix.stdin w1 w2 in
    List.iter Unix.close [w1;w2];
    { outbuf = Buffer.create 1024
    ; errbuf = Buffer.create 1024
    ; fd_out = r1
    ; fd_err = r2
    ; pid    = pid
    }

let run_with_outputs gconf args =
    verbose gconf DebugPlus "  [CMD]: %s\n%!" (String.concat " " args);

    let exe_state = spawn args in

    let readfds = ref [exe_state.fd_out;exe_state.fd_err] in
    let b = String.create 1024 in
    while !readfds <> []
    do
        let (rs, _, _) = Unix.select !readfds [] [] 2.0 in
        if List.mem exe_state.fd_out rs then (
            let nb = Unix.read exe_state.fd_out b 0 1024 in
            if nb > 0
                then Buffer.add_substring exe_state.outbuf b 0 nb
                else readfds := List.filter (fun x -> x <> exe_state.fd_out) !readfds
        );
        if List.mem exe_state.fd_err rs then (
            let nb = Unix.read exe_state.fd_err b 0 1024 in
            if nb > 0
                then Buffer.add_substring exe_state.errbuf b 0 nb
                else readfds := List.filter (fun x -> x <> exe_state.fd_err) !readfds
        );
    done;

    let (_, pstat) = Unix.waitpid [] exe_state.pid in
    match pstat with
    | Unix.WEXITED 0 -> Success (Buffer.contents exe_state.outbuf, Buffer.contents exe_state.errbuf)
    | Unix.WEXITED n -> Failure (Buffer.contents exe_state.errbuf)
    | _              -> Failure ""

let scheduler () = ()
