open Types
open Ext
open Printf
open Filepath
open Modname
open Hier
open Target
open Dependencies
open Gconf

exception NoConfFile
exception MultipleConfFiles
exception InvalidConfFile of string
exception MissingField of string
exception UnknownDependencyName of string
exception UnsupportedFutureVersion of int
exception ModuleDoesntExist of target * modname
exception ModuleListEmpty of lib_name
exception FileDoesntExist of target * filename
exception LicenseFileDoesntExist of filepath
exception BlockSectionAsValue of string
exception ExecutableWithNoMain of string
exception UnknownStdlib of string
exception UnknownExtraDepFormat of string
exception UnknownFlag of string

type obuild_lib =
    { lib_name        : lib_name
    ; lib_description : string
    ; lib_target      : target
    ; lib_modules     : modname list
    ; lib_pack        : bool
    ; lib_syntax      : bool
    ; lib_subs        : obuild_lib list
    }

type obuild_exe =
    { exe_name      : exe_name
    ; exe_main      : filename
    ; exe_target    : target
    }

type test_type = TestType_ExitCode

type obuild_test =
    { test_name     : exe_name
    ; test_main     : filename
    ; test_target   : target
    ; test_rundir   : filepath option
    ; test_runopt   : string list
    ; test_type     : test_type
    }

type obuild_bench =
    { bench_name     : exe_name
    ; bench_main     : filename
    ; bench_target   : target
    (* TODO add bench type *)
    }

(* an example is an executable that doesn't get installed.
 * or maybe install in a documentation directory
 *)
type obuild_example =
    { example_name     : exe_name
    ; example_main     : filename
    ; example_target   : target
    }

let lib_to_target obj = obj.lib_target
let exe_to_target obj = obj.exe_target
let test_to_target obj = obj.test_target
let bench_to_target obj = obj.bench_target
let example_to_target obj = obj.example_target

let rec lib_to_targets lib =
    lib.lib_target :: List.concat (List.map lib_to_targets lib.lib_subs)

let rec lib_flatten lib : obuild_lib list =
    lib :: List.concat (List.map lib_flatten lib.lib_subs)

type obuild_flag =
    { flag_name        : string
    ; flag_description : string
    ; flag_default     : bool option
    }

type obuild =
    { name        : string
    ; version     : string
    ; synopsis    : string
    ; description : string
    ; license     : string
    ; license_file: filepath option
    ; authors     : string list
    ; obuild_ver  : int
    ; homepage    : string
    ; flags       : obuild_flag list
    ; libs        : obuild_lib list
    ; exes        : obuild_exe list
    ; tests       : obuild_test list
    ; benchs      : obuild_bench list
    ; examples    : obuild_example list
    ; extra_srcs  : filepath list
    }

let emptyObuild =
    { name        = ""
    ; version     = ""
    ; synopsis    = ""
    ; description = ""
    ; license     = ""
    ; license_file= None
    ; authors     = []
    ; obuild_ver  = 0
    ; homepage    = ""
    ; flags       = []
    ; libs        = []
    ; exes        = []
    ; tests       = []
    ; benchs      = []
    ; examples    = []
    ; extra_srcs  = []
    }

let emptyLibLname lname : obuild_lib =
    { lib_name        = lname
    ; lib_description = ""
    ; lib_modules     = []
    ; lib_pack        = false
    ; lib_syntax      = false
    ; lib_target      = newTarget (LibName lname) Lib true true
    ; lib_subs        = []
    }

let emptyLibPrefix libname subname = emptyLibLname (lib_name_append libname subname)
let emptyLib libname = emptyLibLname (lib_name_of_string libname)

let emptyExe (name : string) : obuild_exe =
    { exe_name   = name
    ; exe_main   = { filename = "" }
    ; exe_target = newTarget (ExeName name) Exe true false
    }

let emptyTest (name : string) : obuild_test =
    { test_name   = name
    ; test_main   = { filename = "" }
    ; test_target = newTarget (TestName name) Test false false
    ; test_rundir = None
    ; test_runopt = []
    ; test_type   = TestType_ExitCode
    }

let emptyExample (name : string) : obuild_example =
    { example_name   = name
    ; example_main   = { filename = "" }
    ; example_target = newTarget (ExampleName name) Test gconf.conf_build_examples false
    }

let findPath () =
    let ents = Array.to_list (Sys.readdir ".") in
    match List.find_all (fun ent -> not (string_startswith "." ent) && string_endswith ".obuild" ent) ents with
    | []  -> raise NoConfFile
    | [x] -> fp x
    | _   -> raise MultipleConfFiles

let digest () =
    let path = findPath () in
    Digest.to_hex (Digest.file (fp_to_string path))

type 'a optionHandling = Handled of 'a | NotHandled

let parse strict lines =
    let raise_if_strict s =
        if strict then raise (InvalidConfFile s)
                  else Printf.eprintf "config warning: %s\n" s
        in
    let rec getBlock (lvl: int) (lines: (int * string) list): ((int * string) list * (int * string) list) =
        match lines with
        | []              -> ([], [])
        | (clvl, line)::ls ->
                if lvl < clvl
                    then let (b1, b2) = getBlock lvl ls in
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
        match lines with
        | [] -> acc
        | (lvl, line)::ls ->
            let (cont,rem) = getBlock lvl ls in
            let nacc = f acc line cont in
            processChunk f nacc rem
        in
    let parseFlag (acc: obuild_flag) line cont : obuild_flag =
        match Utils.toKV line with
        | (k, None)   -> raise_if_strict ("unexpected item in flag " ^ k); acc
        | (k, Some v) ->
                let (value: string) = String.concat "\n" (v :: List.map snd cont) in
                (match String.lowercase k with
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
    let parseDeps keyParse value =
        let parseConstraint l =
            (* TODO proper lexer/parser to parse real expressions. *)
            None
            in
        let parseDependency w =
            match string_words_noempty w with
            | []     -> failwith "empty dependency"
            | k :: l -> let depname = keyParse k in
                        (depname, parseConstraint l)
            in
        List.map parseDependency (Utils.parseCSV value)
        in
    let parseModuleName value =
        let wrap_module_nice s = wrap_module (String.capitalize s) in
        List.map wrap_module_nice (Utils.parseCSV value)
        in
    let parseFilenames value = List.map fn (Utils.parseCSV value) in
    let parseExtraDep value =
        let vs = Utils.parseCSV value in
        List.map (fun v ->
            match string_words v with
            | [h1; "then"; h2] | [h1; "before"; h2] | [h1; "->"; h2] | [h1; h2] ->
                (hier_of_string h1, hier_of_string h2)
            | _              -> raise (UnknownExtraDepFormat v)
        ) vs
        in
    let parse_stdlib value =
        match String.lowercase value with
        | "none" | "no" -> Stdlib_None
        | "standard"    -> Stdlib_Standard
        | "core"        -> Stdlib_Core
        | _             -> raise (UnknownStdlib value)
        in

    let parseTargetObits t k value =
        match k with
        | "builddepends"
        | "build-deps" -> Handled { t with target_builddeps = parseDeps lib_name_of_string value @ t.target_builddeps }
        | "path" | "srcdir"
        | "src-dir"    -> Handled { t with target_srcdir    = fp value }
        | "preprocessor"
        | "pp"         -> Handled { t with target_pp = Some (Pp.pp_type_of_string value) }
        | "extra-deps" -> Handled { t with target_extradeps = t.target_extradeps @ parseExtraDep value }
        | "stdlib"     -> Handled { t with target_stdlib = parse_stdlib value }
        | _            -> NotHandled
        in

    let parseTargetCbits t k value =
        match k with
        | "cdir"
        | "c-dir"      -> Handled { t with target_cdir      = fp value }
        | "csources"
        | "c-sources"  -> Handled { t with target_csources  = t.target_csources  @ parseFilenames value }
        | "cflags" | "c-flags" | "ccopts" | "ccopt"
        | "c-opts"     -> Handled { t with target_cflags    = t.target_cflags    @ string_words_noempty value }
        | "c-libpaths" -> Handled { t with target_clibpaths = t.target_clibpaths @ List.map fp (string_words_noempty value) }
        | "c-libs"     -> Handled { t with target_clibs     = t.target_clibs     @ string_words_noempty value }
        | "c-pkgs"     -> Handled { t with target_cpkgs     = t.target_cpkgs     @ parseDeps id value }
        | _            -> NotHandled
        in

    let parse_runtime_bool ctx value =
        match value with
        | "true" | "True"   -> BoolConst true
        | "false" | "False" -> BoolConst false
        | flag              -> if string_startswith "$" flag then BoolVariable (string_drop 1 flag) else BoolVariable flag
        in
    let parseTarget t k value =
        match k with
        | "buildable"   -> { t with target_buildable = parse_runtime_bool "buildable" value }
        | "installable" -> { t with target_installable = parse_runtime_bool "installable" value }
        | k             ->
            match parseTargetObits t.target_obits k value with
            | Handled nobits -> { t with target_obits = nobits }
            | NotHandled ->
                match parseTargetCbits t.target_cbits k value with
                | Handled ncbits -> { t with target_cbits = ncbits }
                | NotHandled     ->
                    raise_if_strict ("unexpected item in : " ^ k); t
        in
    let doBlock ty mempty parseM accu args cont =
        let name = getContextName ty args in
        accu (processChunk parseM (mempty name) cont)
        in

    (*************    library parsing     ***************************)
    let rec parseLibrary (acc: obuild_lib) line cont =
        match Utils.toKV line with
        | (k, None)   ->
            (match string_words_noempty k with
            | []                -> raise_if_strict ("unknown empty block in library"); acc
            | blockName :: args ->
                match String.lowercase blockName with
                | "sub" | "sublib" | "library" -> (
                    let doSub = doBlock "library" (emptyLibPrefix acc.lib_name) parseLibrary
                                      (fun obj -> { acc with lib_subs = obj :: acc.lib_subs})
                        in
                    doSub args cont
                    )
                | _                            -> raise_if_strict ("unexpected block name in library: " ^ blockName); acc
            )
        | (k, Some v) ->
            let (value: string) = String.concat "\n" (v :: List.map snd cont) in
            (match String.lowercase k with
            | "modules"     -> { acc with lib_modules = parseModuleName value @ acc.lib_modules }
            | "pack"        -> { acc with lib_pack    = user_bool_of_string "pack" value }
            | "syntax"      -> { acc with lib_syntax  = user_bool_of_string "syntax" value }
            | "description" -> { acc with lib_description = value }
            | "sub" | "sublib" | "library" -> raise (BlockSectionAsValue k)
            | k             -> { acc with lib_target  = parseTarget acc.lib_target k value }
            )
        in
    let doLibrary acc = doBlock "library" emptyLib parseLibrary
                               (fun obj -> { acc with libs = obj :: acc.libs })
        in
    (*************    executable parsing    *************************)
    let parseExecutabloid sectionName setMain setTarget myTarget other acc line cont =
        match Utils.toKV line with
        | (k, None) -> raise_if_strict ("unexpected item in " ^ sectionName ^ " " ^ k); acc
        | (k, Some v) ->
            let (value: string) = String.concat "\n" (v :: List.map snd cont) in
            (match String.lowercase k with
            | "main" | "mainis"
            | "main-is"    -> setMain acc (fn value)
            | k            ->
                try let f = List.assoc k other in f acc value
                with Not_found -> setTarget acc (parseTarget myTarget k value)
            )
        in

    let parseExecutable obj =
        parseExecutabloid "executable" (fun acc main -> { acc with exe_main = main })
                                       (fun acc target -> { acc with exe_target = target })
                                       obj.exe_target
                                       []
                                       obj
        in
    let parseTest obj =
        parseExecutabloid "test" (fun acc main -> { acc with test_main = main })
                                 (fun acc target -> { acc with test_target = target })
                                 obj.test_target
                                 [ ("rundir", (fun acc v -> { acc with test_rundir = Some (fp v) }))
                                 ; ("runopt", (fun acc v -> { acc with test_runopt = acc.test_runopt @ string_words v }))
                                 ]
                                 obj
        in
    let parseExample obj =
        parseExecutabloid "example" (fun acc main -> { acc with example_main = main })
                                    (fun acc target -> { acc with example_target = target })
                                    obj.example_target
                                    []
                                    obj
        in
    let doExample acc = doBlock "example" emptyExample parseExample
                               (fun obj -> { acc with examples = obj :: acc.examples })
        in
    let doTest acc = doBlock "test" emptyTest parseTest
                             (fun obj -> { acc with tests = obj :: acc.tests })
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
            | blockName :: args    ->
                match String.lowercase blockName with
                | "executable" -> doExecutable acc args cont
                | "library"    -> doLibrary acc args cont
                | "flag"       -> doFlag acc args cont
                | "test"       -> doTest acc args cont
                | "bench"      -> raise_if_strict ("unimplemented section: " ^ blockName); acc
                | "example"    -> doExample acc args cont
                | _            -> raise_if_strict ("unknown block name: " ^ blockName); acc
            )
        | (k, Some v) -> (
            let (value: string) = String.concat "\n" (v :: List.map snd cont) in
            match String.lowercase k with
            | "name"        -> { acc with name = value }
            | "version"     -> { acc with version = value }
            | "synopsis"    -> { acc with synopsis = value }
            | "description" -> { acc with description = value }
            | "license"
            | "licence"     -> { acc with license = value }
            | "license-file"
            | "licence-file" -> { acc with license_file = Some (fp value) }
            | "homepage"    -> { acc with homepage = value }
            | "authors"     -> { acc with authors = Utils.parseCSV value }
            | "author"      -> { acc with authors = [value] }
            | "extra-srcs"  -> { acc with extra_srcs = List.map fp (Utils.parseCSV value) @ acc.extra_srcs }
            | "obuild-ver"  -> { acc with obuild_ver = user_int_of_string "obuild-ver" value }
            (* for better error reporting *)
            | "executable" | "library" | "test" | "bench" | "example" -> raise (BlockSectionAsValue k)
            | k             -> raise_if_strict ("unknown key: " ^ k); acc
        )
        in
    processChunk parseRoot emptyObuild lines

let check proj =
    (if proj.name = "" then raise (MissingField "name"));
    (if proj.version = "" then raise (MissingField "version"));
    (if proj.obuild_ver = 0 then raise (MissingField "obuild-ver"));
    (if proj.obuild_ver > 1 then raise (UnsupportedFutureVersion proj.obuild_ver));

    let check_files_exists target names =
        let srcdir = target.target_obits.target_srcdir in
        List.iter (fun n ->
            let path = srcdir </> n in
            if not (Filesystem.exists path)
                then raise (FileDoesntExist (target, n))
        ) names
        in

    let check_modules_exists target modules =
        let srcdir = target.target_obits.target_srcdir in
        List.iter (fun m ->
            if not (Utils.exist_choice_in_paths [srcdir] (List.map (fun f -> f m) Modname.module_lookup_methods))
                then raise (ModuleDoesntExist (target, m))
        ) modules
        in

    let check_modules_not_empty lib =
        if lib.lib_modules = []
            then raise (ModuleListEmpty (lib.lib_name))
        in

    maybe_unit (fun x -> if not (Filesystem.exists x) then raise (LicenseFileDoesntExist x)) proj.license_file;

    (* check sublibs in libs *)
    List.iter (fun rootlib ->
        check_modules_not_empty rootlib;
        let sublibs = lib_flatten rootlib in
        List.iter (fun lib ->
            check_modules_not_empty lib;
            check_modules_exists lib.lib_target lib.lib_modules) sublibs
    ) proj.libs;

    List.iter (fun exe ->
        if fn_to_string exe.exe_main = ""
            then raise (ExecutableWithNoMain exe.exe_name);
        check_files_exists exe.exe_target [exe.exe_main]
    ) proj.exes;
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
            else (if s.[!p] = '#' then None else Some (!p, string_drop !p s))
        in
    let path = findPath () in
    let lines = Utils.read_file_with (fun s -> countSpacesAndTrim s) (fp_to_string path) in
    let proj = parse strict lines in
    check proj;
    proj

let write file proj =
    Utils.generateFile file (fun add ->
        let add_string k s = if s <> "" then add (sprintf "%s: %s\n" k s) in

        add (sprintf "name: %s\n" proj.name);
        add (sprintf "version: %s\n" proj.version);
        add_string "synopsis" proj.synopsis;
        add_string "description" proj.description;
        add_string "license" proj.license;
        add_string "homepage" proj.homepage;
        maybe () (fun x -> add_string "license-file" (fp_to_string x)) proj.license_file;
        add_string "authors" (Utils.showList ", " id proj.authors);
        add (sprintf "obuild-ver: %d\n" proj.obuild_ver);

        let show_target iStr target =
            let obits = target.target_obits in
            let cbits = target.target_cbits in
            add (sprintf "%ssrc-dir: %s\n" iStr (fp_to_string obits.target_srcdir));
            add_string (iStr ^ "build-deps") (Utils.showList ", " (fun (l,_) -> lib_name_to_string l) obits.target_builddeps);
            add_string (iStr ^ "oflags") (Utils.showList " " id obits.target_oflags);
            add_string (iStr ^ "pp") (maybe "" (fun ppty -> Pp.pp_type_to_string ppty) obits.target_pp);

            add (sprintf "%sc-dir: %s\n" iStr (fp_to_string cbits.target_cdir));
            add_string (iStr ^ "c-sources") (Utils.showList ", " fn_to_string cbits.target_csources);
            add_string (iStr ^ "c-flags") (Utils.showList " " id cbits.target_cflags);
            add_string (iStr ^ "c-libs") (Utils.showList "," id cbits.target_clibs);
            add_string (iStr ^ "c-libpaths") (Utils.showList "," fp_to_string cbits.target_clibpaths);
            add_string (iStr ^ "c-pkgs") (Utils.showList ", " (fun (l,_) -> l) cbits.target_cpkgs);
            in
        let rec show_lib iStrSection lib = 
            add "\n";
            add (sprintf "%slibrary %s\n" iStrSection (lib_name_to_string lib.lib_name));
            let iStr = iStrSection ^ "  " in
            add (sprintf "%smodules: %s\n" iStr (Utils.showList "," modname_to_string lib.lib_modules));
            if lib.lib_pack then add (sprintf "%spack: %b\n" iStr lib.lib_pack);
            if lib.lib_syntax then add (sprintf "%ssyntax: %b\n" iStr lib.lib_syntax);
            if lib.lib_pack then add (sprintf "%spack: %b\n" iStr lib.lib_pack);
            show_target iStr lib.lib_target;
            List.iter (fun sub -> show_lib iStr sub) lib.lib_subs
            in
        List.iter (show_lib "") proj.libs;
        List.iter (fun exe ->
            add "\n";
            add (sprintf "executable %s\n" exe.exe_name);
            add (sprintf "  main: %s\n" (fn_to_string exe.exe_main));
            show_target "  " exe.exe_target;
            ()
        ) proj.exes;
    )

let get_all_targets projFile =
      List.concat (List.map lib_to_targets projFile.libs)
    @ List.map exe_to_target projFile.exes
    @ List.map test_to_target projFile.tests
    @ List.map bench_to_target projFile.benchs
    @ List.map example_to_target projFile.examples

let find_flag name projFile =
    try Some (List.find (fun fl -> fl.flag_name = name) projFile.flags)
    with Not_found -> None

let get_all_buildable_targets projFile =
    List.filter (fun target ->
        match target.target_buildable with
        | BoolConst t    -> t
        | BoolVariable v ->
            try List.assoc v gconf.conf_user_flags
            with Not_found -> raise (UnknownFlag v)
    ) (get_all_targets projFile)

exception LibraryNotFound of lib_name
exception ExecutableNotFound of exe_name
exception BenchNotFound of exe_name
exception TestNotFound of exe_name
exception ExampleNotFound of exe_name

let find_lib projFile name =
    try List.find (fun l -> l.lib_name = name) (List.concat (List.map lib_flatten projFile.libs))
    with Not_found -> raise (LibraryNotFound name)

let find_exe projFile name =
    try List.find (fun e -> e.exe_name = name) projFile.exes
    with Not_found -> raise (ExecutableNotFound name)

let find_bench projFile name =
    try List.find (fun b -> b.bench_name = name) projFile.benchs
    with Not_found -> raise (BenchNotFound name)

let find_test projFile name =
    try List.find (fun b -> b.test_name = name) projFile.tests
    with Not_found -> raise (TestNotFound name)

let find_example projFile name =
    try List.find (fun b -> b.example_name = name) projFile.examples
    with Not_found -> raise (ExampleNotFound name)
