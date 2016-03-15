open Printf
open Gconf

let print_warnings warnings =
    if warnings <> "" then fprintf stderr "%s\n%!" warnings else ()

let log lvl fmt =
    if lvl <= gconf.verbosity
        then printf fmt
        else ifprintf stdout fmt

let debug fmt = log Gconf.Debug fmt
let report fmt = log Gconf.Report fmt

(* deprecated, replace by other stuff *)
let verbose lvl fmt =
    if lvl <= gconf.verbosity
        then printf fmt
        else ifprintf stdout fmt

let support_color () =
    if Utils.isWindows
        then false
    else if Unix.isatty Unix.stdout
        then Gconf.gconf.color
        else false

let color_red ()   = if support_color () then "\x1b[1;31m" else ""
let color_green () = if support_color () then "\x1b[1;32m" else ""
let color_blue ()  = if support_color () then "\x1b[1;34m" else ""
let color_white () = if support_color () then "\x1b[0m" else ""
