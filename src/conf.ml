open Types

exception NoConfFile
exception MultipleConfFiles

let projectFindPath () =
    let ents = Array.to_list (Sys.readdir ".") in
    match List.find_all (fun ent -> Filename.check_suffix ent ".obuild") ents with
    | []  -> raise NoConfFile
    | [x] -> x
    | _   -> raise MultipleConfFiles

let projectParse lines =
    let rec doBlock lvl lines =
        match lines with
        | []              -> ([], [])
        | (clvl, line)::ls ->
                if lvl < clvl
                    then let (b1, b2) = doBlock lvl ls in
                         (line :: b1, b2)
                    else ([], lines)
        in
    let emptyObuild = { obuild_name        = ""
                      ; obuild_version     = ""
                      ; obuild_description = ""
                      ; obuild_license     = ""
                      ; obuild_author      = ""
                      ; obuild_ver         = 0
                      ; obuild_libs        = []
                      ; obuild_exes        = []
                      } in
    (* process chunks *)
    emptyObuild

let projectRead () =
    let countSpacesAndTrim s =
        let len = String.length s in
        let p = ref 0 in
        while !p < len && s.[!p] = ' ' do
            p := !p + 1
        done;
        if !p = len
            then None
            else Some (!p, Utils.drop !p s)
        in
    let path = projectFindPath () in
    let lines = Utils.read_file_with (fun s -> countSpacesAndTrim s) path in
    projectParse lines
