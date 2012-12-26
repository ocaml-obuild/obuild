open Ext
exception OCamlProgramError of string
exception TarError of string

let getOcamlVersion () =
    match Helper.run_with_outputs [ "ocaml"; "-vnum" ] with
    | Helper.Success (s,_) -> (match string_split ~limit:3 '.' s with
                   | [major;minor;other] ->
                           (user_int_of_string "ocaml version major" major
                           ,user_int_of_string "ocaml version minor" minor
                           ,other
                           )
                   | _ -> raise (OCamlProgramError ("ocaml return an unknown version " ^ s))
                   )
    | Helper.Failure err -> raise (OCamlProgramError err)

let getOcamlConfig useOpt =
    match Helper.run_with_outputs [ (if useOpt then "ocamlopt" else "ocamlc"); "-config" ] with
    | Helper.Success (s,_) -> let lines = string_split '\n' s in
                          List.map (fun z -> second (default "") $ (Utils.toKV z)) lines 
    | Helper.Failure err -> raise (OCamlProgramError ("ocamlopt cannot get config " ^ err))
 
let runTar output dir =
    match Helper.run_with_outputs [ "tar"; "czf"; output; dir ] with
    | Helper.Success _   -> ()
    | Helper.Failure err -> raise (TarError err)
