open Types
open Ext
open Filepath

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
                      ; obuild_authors     = []
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
    let parseDeps value =
        let parseConstraint l =
            (* proper lexer/parser please: to parse real expressions. this is a quicky *)
            match l with
            | []         -> None
            | ["="; v]   -> Some (Eq v)
            | [">="; v ] -> Some (Ge v)
            | ["<="; v ] -> Some (Le v)
            | [">"; v ]  -> Some (Gt v)
            | ["<"; v ]  -> Some (Lt v)
            | _          -> failwith "unrecognize constraint"
            in
        let parseDependency w =
            match string_words w with
            | []     -> failwith "empty dependency"
            | k :: l -> let (dep, subDeps) =
                            match string_split '.' k with
                            | m::subs -> (m, subs)
                            | _       -> failwith ("unknown dependency format: " ^ k)
                            in
                        ({ dep_name = dep; dep_subname = subDeps }, parseConstraint l)
            in
        List.map parseDependency (parseCSV value)
        in
    let parseModuleName value = List.map wrap_module (parseCSV value) in
    let parseFilenames value = List.map wrap_filename (parseCSV value) in

    let parseLibrary (acc: obuild_lib) line cont =
        match Utils.toKV line with
        | (k, None)   -> failwith ("unexpected item in library " ^ k)
        | (k, Some v) ->
                let (value: string) = String.concat "\n" (v :: List.map snd cont) in
                (match k with
                | "modules"    -> { acc with lib_modules   = parseModuleName value @ acc.lib_modules }
                | "build-deps" -> { acc with lib_builddeps = parseDeps value @ acc.lib_builddeps }
                | "src-dir"    -> { acc with lib_srcdir    = Some value }
                | "C-dir"      -> { acc with lib_cdir      = Some value }
                | "C-sources"  -> { acc with lib_csources  = acc.lib_csources @ parseFilenames value }
                | "pack"       -> { acc with lib_pack      = bool_of_string value }
                | k            -> failwith ("unexpected item in library : " ^ k)
                )
        in
    let doLibrary acc args cont =
        let emptyLib = { lib_name = List.hd args
                       ; lib_modules = []
                       ; lib_builddeps = []
                       ; lib_srcdir = None
                       ; lib_csources = []
                       ; lib_cdir = None
                       ; lib_pack = false
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
                | "build-deps" -> { acc with exe_builddeps = parseDeps value @ acc.exe_builddeps }
                | "C-dir"      -> { acc with exe_cdir      = Some value }
                | "C-sources"  -> { acc with exe_csources  = acc.exe_csources @ parseFilenames value }
                | k            -> failwith ("unexpected item in executable : " ^ k)
                )
        in
    let doExecutable acc args cont =
        let emptyExe = { exe_name = List.hd args
                       ; exe_main = ""
                       ; exe_srcdir = None
                       ; exe_builddeps = []
                       ; exe_csources = []
                       ; exe_cdir = None
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
                | "authors"     -> { acc with obuild_authors = parseCSV value }
                | "author"      -> { acc with obuild_authors = [value] }
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
