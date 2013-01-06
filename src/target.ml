open Filepath
open Modname
open Types

type target_type = Lib | Exe | Test | Bench

type target =
    { target_name      : string
    ; target_srcdir    : filepath option
    ; target_cdir      : filepath option
    ; target_csources  : filename list
    ; target_copts     : string list
    ; target_clibs     : string list
    ; target_type      : target_type
    ; target_buildable : bool
    ; target_builddeps : dependency list
    }

let newTarget n ty buildable =
    { target_name      = n
    ; target_buildable = buildable
    ; target_type      = ty
    ; target_builddeps = []
    ; target_srcdir    = None
    ; target_csources  = []
    ; target_copts     = []
    ; target_clibs     = []
    ; target_cdir      = None
    }

let get_target_name target =
    let pre =
        match target.target_type with
        | Exe  -> "exe"
        | Lib  -> "lib"
        | Test -> "test"
        | Bench -> "bench"
        in
    pre ^ "-" ^ target.target_name

let is_target_lib target = target.target_type = Lib

let get_target_filenames target =
    match target.target_type with
    | Lib ->
        [cmxa_of_lib target.target_name; cma_of_lib target.target_name]
    | _   ->
        [Utils.to_exe_name target.target_name]
