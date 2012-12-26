open Types
open Ext

exception NoConfFile
exception MultipleConfFiles
exception MissingField of string

let projectFindPath () =
    let ents = Array.to_list (Sys.readdir ".") in
    match List.find_all (fun ent -> Filename.check_suffix ent ".obuild") ents with
    | []  -> raise NoConfFile
    | [x] -> x
    | _   -> raise MultipleConfFiles

let projectDigest () =
    let path = projectFindPath () in
    Digest.to_hex (Digest.file path)

let projectParse lines =
    let rec doBlock (lvl: int) (lines: (int * string) list): ((int * string) list * (int * string) list) =
        match lines with
        | []              -> ([], [])
        | (clvl, line)::ls ->
                if lvl < clvl
                    then let (b1, b2) = doBlock lvl ls in
                         ((clvl, line) :: b1, b2)
                    else ([], lines)
        in
    let emptyObuild = { obuild_name        = ""
                      ; obuild_version     = ""
                      ; obuild_description = ""
                      ; obuild_license     = ""
                      ; obuild_author      = ""
                      ; obuild_ver         = 0
                      ; obuild_homepage    = ""
                      ; obuild_flags       = []
                      ; obuild_libs        = []
                      ; obuild_exes        = []
                      } in
    let parseCSV value = List.map string_stripSpaces (string_split ',' value)
        in

    let rec processChunk f acc (lines: (int*string) list) =
        (*doBlock *)
        match lines with
        | [] -> acc
        | (lvl, line)::ls ->
            let (cont,rem) = doBlock lvl ls in
            let nacc = f acc line cont in
            processChunk f nacc rem
        in
    let parseFlag (acc: obuild_flag) line cont =
        match Utils.toKV line with
        | (k, None)   -> failwith ("unexpected item in flag " ^ k)
        | (k, Some v) ->
                let (value: string) = String.concat "\n" (v :: List.map snd cont) in
                (match k with
                | "description" -> { acc with flag_description = value }
                | "default"     -> { acc with flag_default = Some value }
                | k             -> failwith ("unexpected item in flag : " ^ k)
                )
        in
    let doFlag acc args cont =
        let emptyFlag = { flag_name = List.hd args
                        ; flag_description = ""
                        ; flag_default = None
                        } in
        let flag = processChunk parseFlag emptyFlag cont in
        { acc with obuild_flags = flag :: acc.obuild_flags }
        in

    let parseLibrary (acc: obuild_lib) line cont =
        match Utils.toKV line with
        | (k, None)   -> failwith ("unexpected item in library " ^ k)
        | (k, Some v) ->
                let (value: string) = String.concat "\n" (v :: List.map snd cont) in
                (match k with
                | "modules"    -> { acc with lib_modules = parseCSV value @ acc.lib_modules }
                | "build-deps" -> { acc with lib_builddeps = parseCSV value @ acc.lib_builddeps }
                | "src-dir"    -> { acc with lib_srcdir = Some value }
                | k            -> failwith ("unexpected item in library : " ^ k)
                )
        in
    let doLibrary acc args cont =
        let emptyLib = { lib_modules = []
                       ; lib_builddeps = []
                       ; lib_srcdir = None
                       } in
        let lib = processChunk parseLibrary emptyLib cont in
        { acc with obuild_libs = lib :: acc.obuild_libs }
        in
    let parseExecutable (acc: obuild_exe) line cont =
        match Utils.toKV line with
        | (k, None) -> failwith ("unexpected item in executable " ^ k)
        | (k, Some v) ->
                let (value: string) = String.concat "\n" (v :: List.map snd cont) in
                (match k with
                | "src-dir"    -> { acc with exe_srcdir = Some value }
                | "main-is"    -> { acc with exe_main = value }
                | "build-deps" -> { acc with exe_builddeps = parseCSV value @ acc.exe_builddeps }
                | k            -> failwith ("unexpected item in executable : " ^ k)
                )
        in
    let doExecutable acc args cont =
        let emptyExe = { exe_name = List.hd args
                       ; exe_main = ""
                       ; exe_srcdir = None
                       ; exe_builddeps = []
                       } in
        let exe = processChunk parseExecutable emptyExe cont in
        { acc with obuild_exes = exe :: acc.obuild_exes }
        in
    (* process chunks *)
    let parseRoot (acc: obuild) (line: string) (cont: (int*string) list) =
        match Utils.toKV line with
        | (k, None) -> (
                        match string_words k with
                        | []                   -> assert false
                        | "executable" :: args -> doExecutable acc args cont
                        | "library" :: args    -> doLibrary acc args cont
                        | "flag" :: args       -> doFlag acc args cont
                        | blockName :: args    -> failwith ("unknown block name: " ^ blockName)
                       )
        | (k, Some v) -> (
                let (value: string) = String.concat "\n" (v :: List.map snd cont) in
                match k with
                | "name"        -> { acc with obuild_name = value }
                | "version"     -> { acc with obuild_version = value }
                | "description" -> { acc with obuild_description = value }
                | "license"     -> { acc with obuild_license = value }
                | "licence"     -> { acc with obuild_license = value }
                | "homepage"    -> { acc with obuild_homepage = value }
                | "author"      -> { acc with obuild_author = value }
                | "obuild-ver"  -> { acc with obuild_ver = user_int_of_string "obuild-ver" value }
                | k             -> failwith ("unknown key: " ^ k)
        )
        in
    processChunk parseRoot emptyObuild lines

let projectCheck proj =
    (if proj.obuild_name = "" then raise (MissingField "name"));
    (if proj.obuild_version = "" then raise (MissingField "version"));
    (if proj.obuild_ver = 0 then raise (MissingField "obuild-ver"));
    ()

let projectRead () =
    let countSpacesAndTrim s =
        let len = String.length s in
        let p = ref 0 in
        while !p < len && s.[!p] = ' ' do
            p := !p + 1
        done;
        if !p = len
            then None
            else Some (!p, string_drop !p s)
        in
    let path = projectFindPath () in
    let lines = Utils.read_file_with (fun s -> countSpacesAndTrim s) path in
    let proj = projectParse lines in
    projectCheck proj;
    proj
