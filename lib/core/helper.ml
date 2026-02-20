open Printf
open Gconf

let print_warnings warnings =
    if warnings <> "" then fprintf stderr "%s\n%!" warnings else ()

let log lvl fmt =
    if lvl <= gconf.verbosity
        then printf fmt
        else ifprintf stdout fmt

(* backward compatibility alias *)
let verbose = log

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

(** Performance timing utilities *)
module Timing = struct
  let measure_time name f =
    let start = Unix.gettimeofday () in
    let result = f () in
    let elapsed = Unix.gettimeofday () -. start in
    log Gconf.Debug "[TIMING] %s: %.3fs\n" name elapsed;
    result

  let measure_time_verbose name f =
    let start = Unix.gettimeofday () in
    log Gconf.Debug "[TIMING] %s: starting...\n" name;
    let result = f () in
    let elapsed = Unix.gettimeofday () -. start in
    log Gconf.Debug "[TIMING] %s: completed in %.3fs\n" name elapsed;
    result
end
