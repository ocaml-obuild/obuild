open Ext
open Gconf

exception OCamlProgramError of string
exception TarError of string

let maybe_with_opt s = s ^ (if gconf.conf_withopt then ".opt" else "")
let getOcamlOpt () = default (maybe_with_opt "ocamlopt") gconf.conf_prog_ocamlopt
let getOcamlC   () = default (maybe_with_opt "ocamlc") gconf.conf_prog_ocamlc
let getOcamlDep () = default (maybe_with_opt "ocamldep") gconf.conf_prog_ocamldep
let getCC       () = default "gcc" gconf.conf_prog_cc
let getRanlib   () = default "ranlib" gconf.conf_prog_ranlib
let getAR       () = default "ar" gconf.conf_prog_ar
let getLD       () = default "ld" gconf.conf_prog_ld

let getOcamlVersion () =
    match Process.run_with_outputs [ getOcamlOpt (); "-vnum" ] with
    | Process.Success (s,_) -> (match string_split ~limit:3 '.' s with
                   | [major;minor;other] ->
                           (user_int_of_string "ocaml version major" major
                           ,user_int_of_string "ocaml version minor" minor
                           ,other
                           )
                   | _ -> raise (OCamlProgramError ("ocaml return an unknown version " ^ s))
                   )
    | Process.Failure err -> raise (OCamlProgramError err)

let getOcamlConfig () =
    match Process.run_with_outputs [ getOcamlOpt (); "-config" ] with
    | Process.Success (s,_) ->
        let lines = string_lines_noempty s in
        List.map (fun z -> second (default "") $ (Utils.toKV z)) lines
    | Process.Failure err -> raise (OCamlProgramError ("ocamlopt cannot get config " ^ err))
 
let runTar output dir =
    match Process.run_with_outputs [ "tar"; "czf"; output; dir ] with
    | Process.Success _   -> ()
    | Process.Failure err -> raise (TarError err)
