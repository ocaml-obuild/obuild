open Types
open Ext
open Filepath
open Modname
open Target

exception NoConfFile
exception MultipleConfFiles
exception InvalidConfFile of string
exception MissingField of string
exception UnknownDependencyName of string
exception UnsupportedFutureVersion of int
exception SublibNotCorrect of string * (modname list) * (filename list)

type obuild_sublib =
    { sublib_name        : string
    ; sublib_description : string
    ; sublib_modules     : modname list
    ; sublib_csources    : filename list
    }

type obuild_lib =
    { lib_name        : string
    ; lib_description : string
    ; lib_target      : target
    ; lib_modules     : modname list
    ; lib_pack        : bool
    ; lib_subs        : obuild_sublib list
    }

type obuild_exe =
    { exe_name      : string
    ; exe_main      : string
    ; exe_target    : target
    }

type obuild_test =
    { test_name     : string
    ; test_main     : string
    ; test_target   : target
    }

let lib_to_target lib = lib.lib_target
let exe_to_target exe = exe.exe_target
let test_to_target test = test.test_target

type obuild_flag =
    { flag_name        : string
    ; flag_description : string
    ; flag_default     : bool option
    }

type obuild =
    { name        : string
    ; version     : string
    ; description : string
    ; license     : string
    ; authors     : string list
    ; obuild_ver  : int
    ; homepage    : string
    ; flags       : obuild_flag list
    ; libs        : obuild_lib list
    ; exes        : obuild_exe list
    ; tests       : obuild_test list
    }

let emptyObuild =
    { name        = ""
    ; version     = ""
    ; description = ""
    ; license     = ""
    ; authors     = []
    ; obuild_ver  = 0
    ; homepage    = ""
    ; flags       = []
    ; libs        = []
    ; exes        = []
    ; tests       = []
    }

let emptyLib name =
    { lib_name        = name
    ; lib_description = ""
    ; lib_modules     = []
    ; lib_pack        = false
    ; lib_target      = newTarget name Lib true
    ; lib_subs        = []
    }

let emptySubLib name =
    { sublib_name        = name
    ; sublib_description = ""
    ; sublib_modules     = []
    ; sublib_csources    = []
    }

let emptyExe name =
    { exe_name   = name
    ; exe_main   = ""
    ; exe_target = newTarget name Exe true
    }

let findPath () =
    let ents = Array.to_list (Sys.readdir ".") in
    match List.find_all (fun ent -> Filename.check_suffix ent ".obuild") ents with
    | []  -> raise NoConfFile
    | [x] -> x
    | _   -> raise MultipleConfFiles

let digest () =
    let path = findPath () in
    Digest.to_hex (Digest.file path)

let dep_name_of_string s =
    match string_split '.' s with
    | m::subs -> { dep_name = m; dep_subname = subs }
    | _       -> raise (UnknownDependencyName s)

let dep_name_to_string dn = String.concat "." (dn.dep_name :: dn.dep_subname)

let parse strict lines =
    let raise_if_strict s =
        if strict then raise (InvalidConfFile s)
        in
    let rec doBlock (lvl: int) (lines: (int * string) list): ((int * string) list * (int * string) list) =
        match lines with
        | []              -> ([], [])
        | (clvl, line)::ls ->
                if lvl < clvl
                    then let (b1, b2) = doBlock lvl ls in
                         ((clvl, line) :: b1, b2)
                    else ([], lines)
        in
    let getContextName ctxName args =
        match args with
        | [name] -> name
        | []     -> failwith (ctxName ^ " need a name")
        | _      -> failwith (ctxName ^ " has too many arguments, expecting just a name")
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
    let parseFlag (acc: obuild_flag) line cont : obuild_flag =
        match Utils.toKV line with
        | (k, None)   -> raise_if_strict ("unexpected item in flag " ^ k); acc
        | (k, Some v) ->
                let (value: string) = String.concat "\n" (v :: List.map snd cont) in
                (match k with
                | "description" -> { acc with flag_description = value }
                | "default"     -> { acc with flag_default = Some (user_bool_of_string "flag default" value) }
                | k             -> (raise_if_strict ("unexpected item in flag : " ^ k); acc)
                )
        in
    let doFlag acc args cont =
        let emptyFlag = { flag_name = getContextName "flag" args
                        ; flag_description = ""
                        ; flag_default = None
                        } in
        let flag = processChunk parseFlag emptyFlag cont in
        { acc with flags = flag :: acc.flags }
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
            match string_words_noempty w with
            | []     -> failwith "empty dependency"
            | k :: l -> let depname = dep_name_of_string k in
                        (depname, parseConstraint l)
            in
        List.map parseDependency (Utils.parseCSV value)
        in
    let parseModuleName value = List.map wrap_module (Utils.parseCSV value) in
    let parseFilenames value = List.map wrap_filename (Utils.parseCSV value) in

    let parseTarget t k value =
        match k with
        | "build-deps" -> { t with target_builddeps = parseDeps value @ t.target_builddeps }
        | "src-dir"    -> { t with target_srcdir    = Some (fp value) }
        | "C-dir"      -> { t with target_cdir      = Some (fp value) }
        | "C-sources"  -> { t with target_csources  = t.target_csources @ parseFilenames value }
        | "C-opts"     -> { t with target_copts     = t.target_copts    @ string_words_noempty value }
        | "C-libs"     -> { t with target_clibs     = t.target_clibs    @ string_words_noempty value }
        | "buildable"  -> { t with target_buildable = user_bool_of_string "buildable" value }
        | k            -> raise_if_strict ("unexpected item in : " ^ k); t
        in
    let doBlock ty mempty parseM accu args cont =
        let name = getContextName ty args in
        accu (processChunk parseM (mempty name) cont)
        in

    (*************    library parsing     ***************************)
    let parseSubLibrary (acc: obuild_sublib) line cont =
        match Utils.toKV line with
        | (k, None)   -> raise_if_strict ("unexpected item in sublibrary " ^ k); acc
        | (k, Some v) ->
                let (value: string) = String.concat "\n" (v :: List.map snd cont) in
                (match k with
                | "modules"     -> { acc with sublib_modules  = parseModuleName value @ acc.sublib_modules }
                | "C-sources"   -> { acc with sublib_csources = acc.sublib_csources @ parseFilenames value }
                | "description" -> { acc with sublib_description = value }
                | k             -> raise_if_strict ("unexpected item in sublibrary: " ^ k); acc
                )
        in
    let doSubLibrary acc = doBlock "sublibrary" emptySubLib parseSubLibrary
                               (fun obj -> { acc with lib_subs = obj :: acc.lib_subs })
        in
    let parseLibrary (acc: obuild_lib) line cont =
        match Utils.toKV line with
        | (k, None)   ->
            (match string_words_noempty k with
            | []               -> raise_if_strict ("unknown empty block in library"); acc
            | "sub" :: args    -> doSubLibrary acc args cont
            | _                -> raise_if_strict ("unexpected item in library " ^ k); acc
            )
        | (k, Some v) ->
            let (value: string) = String.concat "\n" (v :: List.map snd cont) in
            (match k with
            | "modules"     -> { acc with lib_modules = parseModuleName value @ acc.lib_modules }
            | "pack"        -> { acc with lib_pack    = user_bool_of_string "pack" value }
            | "description" -> { acc with lib_description = value }
            | k             -> { acc with lib_target  = parseTarget acc.lib_target k value }
            )
        in
    let doLibrary acc = doBlock "library" emptyLib parseLibrary
                               (fun obj -> { acc with libs = obj :: acc.libs })
        in
    (*************    executable parsing    *************************)
    let parseExecutable (acc: obuild_exe) line cont =
        match Utils.toKV line with
        | (k, None) -> raise_if_strict ("unexpected item in executable " ^ k); acc
        | (k, Some v) ->
            let (value: string) = String.concat "\n" (v :: List.map snd cont) in
            (match k with
            | "main-is"    -> { acc with exe_main   = value }
            | k            -> { acc with exe_target = parseTarget acc.exe_target k value }
            )
        in
    let doExecutable acc = doBlock "executable" emptyExe parseExecutable
                               (fun obj -> { acc with exes = obj :: acc.exes })
        in
    (*************    root parsing    *******************************)
    let parseRoot (acc: obuild) (line: string) (cont: (int*string) list) =
        match Utils.toKV line with
        | (k, None) ->
            (match string_words_noempty k with
            | []                   -> raise_if_strict ("unknown empty block"); acc
            | "executable" :: args -> doExecutable acc args cont
            | "library" :: args    -> doLibrary acc args cont
            | "flag" :: args       -> doFlag acc args cont
            | blockName :: args    -> raise_if_strict ("unknown block name: " ^ blockName); acc
            )
        | (k, Some v) -> (
            let (value: string) = String.concat "\n" (v :: List.map snd cont) in
            match k with
            | "name"        -> { acc with name = value }
            | "version"     -> { acc with version = value }
            | "description" -> { acc with description = value }
            | "license"     -> { acc with license = value }
            | "licence"     -> { acc with license = value }
            | "homepage"    -> { acc with homepage = value }
            | "authors"     -> { acc with authors = Utils.parseCSV value }
            | "author"      -> { acc with authors = [value] }
            | "obuild-ver"  -> { acc with obuild_ver = user_int_of_string "obuild-ver" value }
            | k             -> raise_if_strict ("unknown key: " ^ k); acc
        )
        in
    processChunk parseRoot emptyObuild lines

let check proj =
    (if proj.name = "" then raise (MissingField "name"));
    (if proj.version = "" then raise (MissingField "version"));
    (if proj.obuild_ver = 0 then raise (MissingField "obuild-ver"));
    (if proj.obuild_ver > 1 then raise (UnsupportedFutureVersion proj.obuild_ver));

    (* check sublibs in libs *)
    List.iter (fun lib ->
        List.iter (fun sublib ->
            let notRefedModules =
                List.find_all (fun m ->
                    List.mem m lib.lib_modules  
                ) sublib.sublib_modules
                in
            let notRefedCsources =
                List.find_all (fun c ->
                    List.mem c lib.lib_target.target_csources
                ) sublib.sublib_csources
                in
            if notRefedModules <> [] || notRefedCsources <> []
                then raise (SublibNotCorrect (sublib.sublib_name, notRefedModules, notRefedCsources))
        ) lib.lib_subs
    ) proj.libs;
    ()

let read strict =
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
    let path = findPath () in
    let lines = Utils.read_file_with (fun s -> countSpacesAndTrim s) path in
    let proj = parse strict lines in
    check proj;
    proj
