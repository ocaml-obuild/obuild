open Printf
open Obuild.Gconf
open Obuild

(* create no-op aliases for all the modules with commands, so they can
 * register themselves (see the Cmd module).
 *)
module Cmd_build = Cmd_build
module Cmd_clean = Cmd_clean
module Cmd_configure = Cmd_configure
module Cmd_doc = Cmd_doc
module Cmd_get = Cmd_get
module Cmd_help = Cmd_help
module Cmd_infer = Cmd_infer
module Cmd_init = Cmd_init
module Cmd_install = Cmd_install
module Cmd_sdist = Cmd_sdist
module Cmd_test = Cmd_test

let printVersion () =
    printf "obuild %s\n" Path_generated.project_version;
    exit 0

let global_args = Arg.align [
    ("--version", Arg.Unit printVersion, " Show the version and exit");
    ("--color", Arg.Unit (fun () -> gconf.color <- true), " enable colors");
    ("-v", Arg.Unit (fun () -> gconf.verbosity <- Verbose), " XXX");
    ("--verbose", Arg.Unit (fun () -> gconf.verbosity <- Verbose), " XXX");
    ("-vv", Arg.Unit (fun () -> gconf.verbosity <- Debug), " XXX");
    ("--debug", Arg.Unit (fun () -> gconf.verbosity <- Debug), " XXX");
    ("-vvv", Arg.Unit (fun () -> gconf.verbosity <- DebugPlus), " XXX");
    ("--debug+", Arg.Unit (fun () -> gconf.verbosity <- DebugPlus), " XXX");
    ("--debug-with-cmd", Arg.Unit (fun () -> gconf.verbosity <- DebugPlus), " XXX");
    ("-q", Arg.Unit (fun () -> gconf.verbosity <- Silent), " XXX");
    ("--silent", Arg.Unit (fun () -> gconf.verbosity <- Silent), " XXX");
    ("--strict", Arg.Unit (fun () -> gconf.strict <- true), " XXX");
    ("--findlib-conf", Arg.String (Gconf.set_env "findlib-path"), " XXX");
    ("--ocamlopt", Arg.String (Gconf.set_env "ocamlopt"), " XXX");
    ("--ocamldep", Arg.String (Gconf.set_env "ocamldep"), " XXX");
    ("--ocamlc", Arg.String (Gconf.set_env "ocamlc"), " XXX");
    ("--cc", Arg.String (Gconf.set_env "cc"), " XXX");
    ("--ar", Arg.String (Gconf.set_env "ar"), " XXX");
    ("--pkg-config", Arg.String (Gconf.set_env "pkgconfig"), " XXX");
    ("--ranlib", Arg.String (Gconf.set_env "ranlib"), " XXX");
]

let defaultMain () =
    let args = ref global_args in
    let argv = ref [] in
    let cmd = ref None in
    let anon_fun arg =
        match !cmd with
        | None ->
            (* got a command, so switch the args to the command ones *)
            let new_cmd = Cmd.require_cmd arg in
            cmd := Some new_cmd;
            args := Arg.align new_cmd.Cmd.args;
        | Some _ ->
            (* pile up the arguments for the command *)
            argv := arg :: !argv
    in
    let usage_msg = sprintf "See `%s --help'." Cmd.programName in
    Arg.parse_dynamic args anon_fun usage_msg;

    let cmd =
        match !cmd with
        | None ->
            (* no command specified, so show an help *)
            eprintf "Usage: %s <command> [options]\n\nCommands:\n" Cmd.programName;
            let cmds = List.sort compare (Cmd.cmds_list ()) in
            List.iter (
                fun cmd_name ->
                    let cmd = Cmd.find_cmd cmd_name in
                    eprintf "  %-12s %s\n" cmd_name cmd.Cmd.short_desc
            ) cmds;
            exit 1
        | Some cmd -> cmd in

    (* prepare the arguments for the command, and run it *)
    let argv = List.rev !argv in
    cmd.Cmd.fn argv

let () =
    try defaultMain ()
    with
    | Init.ProjectAlreadyExists -> eprintf "error: found another project file in this directory. cannot run init in an already existing project\n"; exit 12
    | Init.AbortedByUser -> eprintf "init aborted. nothing written\n"; exit 0
    | exn -> Exception.show exn
