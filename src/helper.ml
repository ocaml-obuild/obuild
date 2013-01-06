open Types
open Printf
open Gconf

let print_warnings warnings =
    if warnings <> "" then fprintf stderr "%s\n%!" warnings else ()

let verbose lvl fmt =
    if lvl <= gconf.conf_verbosity
        then printf fmt
        else ifprintf stdout fmt
