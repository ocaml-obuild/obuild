open Printf
open Ext
open Obuild.Filepath
open Obuild.Helper
open Obuild.Target
open Obuild.Project
open Obuild.Modname
open Obuild

exception ProjectAlreadyExists
exception CannotRunNotInteractive
exception AbortedByUser

let rec ask v x =
    printf "%s\n> %!" x;
    let r = try read_line () with End_of_file -> raise AbortedByUser in
    match v r with
    | None    -> r
    | Some vp -> printf "error: %s\n" vp; ask v x

let rec ask_many v x =
    let r = ask v x in
    if r = "" then [] else r :: ask_many v x

let run () =
    (* check if a project file already exists and that we run in a interactive windows *)
    (try
        let _ = Project.findPath () in raise ProjectAlreadyExists
    with
        Project.NoConfFile -> ());

    if not (Unix.isatty Unix.stdout)
        then (raise CannotRunNotInteractive);

    printf "  %swelcome to the obuild wizard%s\n" (color_green ()) (color_white());
    printf "  ============================\n";

    let expecting_output l s =
        if List.mem s l
            then None
            else Some (sprintf "expecting one of the following: %s" (Utils.showList ", " (fun s -> "\"" ^ s ^ "\"") l))
        in
            
    let valid_name n = if string_all char_is_alphanum n then None else Some "invalid name" in
    let valid_fp n = None in
    let valid_fn n = if Filepath.valid_fn n then None else Some "invalid filename" in
    let valid_modname = valid_name (* FIXME *) in
    
    let name = ask valid_name "What is the name of your project ?" in
   
    let obuild =
        { Project.emptyObuild with
              Project.name     = name
            ; Project.version  = "0.0.0"
            ; Project.synopsis = "my new project"
            ; Project.obuild_ver = 1
        } in

    let ty = ask (expecting_output ["1";"2"]) "What do you want to build ? 1: executable, 2: library" in

    let question_obits obits =
        let dir = ask valid_fp "What is the directory name where to find the source ? (default .)" in
        { obits with target_srcdir = fp dir }
        in
    let question_cbits cbits =
        cbits
        in

    let project =
        match ty with
        | "1" ->
            let main = ask valid_fn "What is the name of your main ?" in
            let nexe = emptyExe name in
            let itarget = nexe.exe_target in
            let target = { itarget with target_obits = question_obits itarget.target_obits
                                      ; target_cbits = question_cbits itarget.target_cbits } in
            { obuild with
                exes = [ { nexe with exe_main = fn main; exe_target = target } ]
            }
        | "2" ->
            let modules = ask_many valid_modname "Add a module ? (enter to terminate)" in
            let nlib = emptyLib name in
            let itarget = nlib.lib_target in
            let target = { itarget with target_obits = question_obits itarget.target_obits
                                      ; target_cbits = question_cbits itarget.target_cbits } in
            { obuild with
                libs = [ { nlib with lib_modules = List.map wrap_module modules; lib_target = target } ]
            }
        | _ -> assert false
        in
    project
