open Printf
open Ext.Fugue
open Ext.Filepath
open Ext
open Obuild.Helper
open Obuild.Target
open Obuild.Project
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
            
    (* strip [ext] from the the end of [s] only if it's there *)
    let strip_ext s ~ext = 
        try 
            let l = String.length s in
            let ext_l = String.length ext in
            if (String.sub s (l-ext_l) ext_l) = ext
                then String.sub s 0 (l-ext_l)
                else s
        with _ -> s (* in case out of bounds above *)
    in 

    let invalid ~x = function
        | true -> None | false -> Some ("invalid " ^ x) in

    let valid_name n = invalid ~x:"name" (string_all char_is_alphanum n) in
    let valid_fp _ = None in (* FIXME *)
    let valid_fn n = invalid ~x:"filename" (Filepath.valid_fn n) in
    let valid_modname n = invalid ~x:"module name" 
        (string_all Modname.char_is_valid_modchar (strip_ext n ~ext:".ml"))
    in 
    
    let name = ask valid_name "What is the name of your project ?" in
   
    let obuild =
        { Project.make with
              Project.name     = name
            ; Project.version  = "0.0.0"
            ; Project.synopsis = "my new project"
            ; Project.obuild_ver = 1
        } in

    let ty = ask (expecting_output ["1";"2"]) "What do you want to build ? 1: executable, 2: library" in

    let question_obits obits =
        let dir = ask valid_fp "What is the directory name where to find the source ? (default .)" in
        { obits with target_srcdir = [fp dir] }
        in
    let question_cbits cbits =
        cbits
        in

    let project =
      let compose f g x = f (g x) in
        match ty with
        | "1" ->
            let main = ask valid_fn "What is the name of your main ?" in
            let nexe = Executable.make name in
            let itarget = nexe.Executable.target in
            let target = { itarget with target_obits = question_obits itarget.target_obits
                                      ; target_cbits = question_cbits itarget.target_cbits } in
            { obuild with
                exes = [ { nexe with Executable.main = fn main; Executable.target = target } ]
            }
        | "2" ->
            let modules = List.map (fun m -> String.capitalize $ strip_ext ~ext:".ml" m)
                (ask_many valid_modname "Add a module ? (enter to terminate)") in
            let nlib = Library.make_from_string name in
            let itarget = nlib.Library.target in
            let target = { itarget with target_obits = question_obits itarget.target_obits
                                      ; target_cbits = question_cbits itarget.target_cbits } in
            { obuild with
                libs = [ { nlib with Library.modules = List.map (compose Hier.of_modname Modname.wrap) modules; Library.target = target } ]
            }
        | _ -> assert false
        in
    project
