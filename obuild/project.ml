open Ext.Fugue
open Ext.Filepath
open Ext
open Printf
open Target

exception NoConfFile
exception MultipleConfFiles
exception InvalidConfFile of string
exception MissingField of string
exception UnknownDependencyName of string
exception UnsupportedFutureVersion of int
exception ModuleDoesntExist of target * Hier.t
exception ModuleListEmpty of Libname.t
exception FileDoesntExist of target * filename
exception LicenseFileDoesntExist of filepath
exception BlockSectionAsValue of string
exception ExecutableWithNoMain of string
exception UnknownStdlib of string
exception UnknownExtraDepFormat of string
exception UnknownFlag of string
exception BadOcamlVersion of (string * Expr.t)

exception LibraryNotFound of Libname.t
exception ExecutableNotFound of string
exception BenchNotFound of string
exception TestNotFound of string
exception ExampleNotFound of string

type 'a optionHandling = Handled of 'a | NotHandled

let raise_if_strict strict s =
  if strict then raise (InvalidConfFile s)
  else Printf.eprintf "config warning: %s\n" s

let get_context_name ctx_name = function
  | [name] -> name
  | []     -> failwith (ctx_name ^ " need a name")
  | _      -> failwith (ctx_name ^ " has too many arguments, expecting just a name")

let rec get_block (lvl: int) (lines: (int * string) list): ((int * string) list * (int * string) list) =
  match lines with
  | []              -> ([], [])
  | (clvl, line)::ls ->
    if lvl < clvl then
      let (b1, b2) = get_block lvl ls in
      ((clvl, line) :: b1, b2)
    else ([], lines)

let rec process_chunk f acc (lines: (int*string) list) =
  match lines with
  | [] -> acc
  | (lvl, line)::ls ->
    let (cont,rem) = get_block lvl ls in
    let nacc = f acc line cont in
    process_chunk f nacc rem

let do_block ty mempty parseM accu args cont =
  let name = get_context_name ty args in
  accu (process_chunk parseM (mempty name) cont)

let do_block2 _ mempty parseM accu args cont =
  accu (process_chunk parseM (mempty args) cont)

let parse_deps key_parse value =
  let parse_dependency w =
    let (d,c) = Expr.parse_builddep w in
    (key_parse d, c)
  in
  List.map parse_dependency (Utils.parseCSV value)

let parse_extra_dep value =
  let vs = Utils.parseCSV value in
  List.map (fun v ->
      match string_words v with
      | [h1; "then"; h2] | [h1; "before"; h2] | [h1; "->"; h2] | [h1; h2] ->
        (Hier.of_string h1, Hier.of_string h2)
      | _              -> raise (UnknownExtraDepFormat v)
    ) vs

let parse_filenames value = List.map fn (Utils.parseCSV value)

let parse_stdlib value =
  match String.lowercase value with
  | "none" | "no" -> Stdlib_None
  | "standard"    -> Stdlib_Standard
  | "core"        -> Stdlib_Core
  | _             -> raise (UnknownStdlib value)

let parse_runtime_bool _ = function
  | "true" | "True"   -> BoolConst true
  | "false" | "False" -> BoolConst false
  | flag -> if string_startswith "$" flag then BoolVariable (string_drop 1 flag) else BoolVariable flag

let parse_module_name value =
  let wrap_module_nice s = Hier.make [(Modname.wrap (String.capitalize s))] in
  List.map wrap_module_nice (Utils.parseCSV value)

let parse_per strict (acc: target_extra) line cont =
  match Utils.toKV line with
  | (_, None) ->
    raise_if_strict strict ("no block in per"); acc
  | (k, Some v) ->
    let (value: string) = String.concat "\n" (v :: List.map snd cont) in
    match String.lowercase k with
    | "builddepends" | "builddeps"
    | "build-deps" -> { acc with target_extra_builddeps = parse_deps Libname.of_string value @ acc.target_extra_builddeps }
    | "oflags"     -> { acc with target_extra_oflags = acc.target_extra_oflags @ string_words_noempty value }
    | "pp"         -> { acc with target_extra_pp = Some (Pp.Type.of_string value) }
    | _            -> raise_if_strict strict ("unexpected item in : " ^ k); acc

let parse_otarget t k value =
  match k with
  | "builddepends" | "builddeps"
  | "build-deps" -> Handled { t with target_builddeps = parse_deps Libname.of_string value @ t.target_builddeps }
  | "path" | "srcdir"
  | "src-dir"    -> Handled { t with target_srcdir    = List.map (fun p -> fp p) (Utils.parseCSV value) }
  | "preprocessor"
  | "pp"         -> Handled { t with target_pp = Some (Pp.Type.of_string value) }
  | "extra-deps" -> Handled { t with target_extradeps = t.target_extradeps @ parse_extra_dep value }
  | "oflags"     -> Handled { t with target_oflags = t.target_oflags @ string_words_noempty value }
  | "stdlib"     -> Handled { t with target_stdlib = parse_stdlib value }
  | _            -> NotHandled

let parse_ctarget t k value =
  match k with
  | "cdir"
  | "c-dir"      -> Handled { t with target_cdir      = fp value }
  | "csources"
  | "c-sources"  -> Handled { t with target_csources  = t.target_csources  @ parse_filenames value }
  | "cflags" | "c-flags" | "ccopts" | "ccopt"
  | "c-opts"     -> Handled { t with target_cflags    = t.target_cflags    @ string_words_noempty value }
  | "c-libpaths" -> Handled { t with target_clibpaths = t.target_clibpaths @ List.map fp (string_words_noempty value) }
  | "c-libs"     -> Handled { t with target_clibs     = t.target_clibs     @ string_words_noempty value }
  | "c-pkgs"     -> Handled { t with target_cpkgs     = t.target_cpkgs     @ parse_deps id value }
  | _            -> NotHandled

let parse_target strict t k value =
  match k with
  | "buildable"   -> { t with target_buildable = parse_runtime_bool "buildable" value }
  | "installable" -> { t with target_installable = parse_runtime_bool "installable" value }
  | k             ->
    match parse_otarget t.target_obits k value with
    | Handled nobits -> { t with target_obits = nobits }
    | NotHandled ->
      match parse_ctarget t.target_cbits k value with
      | Handled ncbits -> { t with target_cbits = ncbits }
      | NotHandled     ->
        raise_if_strict strict ("unexpected item in : " ^ k); t

module Library = struct
  type t = {
    name        : Libname.t;
    description : string;
    target      : target;
    modules     : Hier.t list;
    pack        : bool;
    syntax      : bool;
    subs        : t list;
  }

  let make name = {
    name;
    description = "";
    modules     = [];
    pack        = false;
    syntax      = false;
    target      = newTarget (Name.Lib name) Typ.Lib true true;
    subs        = []
  }

  let make_prefix libname subname = make (Libname.append libname subname)
  let make_from_string libname = make (Libname.of_string libname)

  let to_target obj = obj.target

  let rec to_targets lib =
    lib.target :: List.concat (List.map to_targets lib.subs)

  let rec flatten lib : t list =
    lib :: List.concat (List.map flatten lib.subs)

  let find libs name =
    try List.find (fun l -> l.name = name) (List.concat (List.map flatten libs))
    with Not_found -> raise (LibraryNotFound name)

  let check_modules_not_empty lib =
    if lib.modules = [] then raise (ModuleListEmpty (lib.name))

  let rec show add show_target section lib =
    add "\n";
    add (sprintf "%slibrary %s\n" section (Libname.to_string lib.name));
    let iStr = section ^ "  " in
    add (sprintf "%smodules: %s\n" iStr (Utils.showList "," Hier.to_string lib.modules));
    if lib.pack then add (sprintf "%spack: %b\n" iStr lib.pack);
    if lib.syntax then add (sprintf "%ssyntax: %b\n" iStr lib.syntax);
    if lib.pack then add (sprintf "%spack: %b\n" iStr lib.pack);
    show_target iStr lib.target;
    List.iter (fun sub -> show add show_target iStr sub) lib.subs

  let rec parse strict acc line cont =
    match Utils.toKV line with
    | (k, None)   ->
      (match string_words_noempty k with
       | []                -> raise_if_strict strict ("unknown empty block in library"); acc
       | blockName :: args ->
         match String.lowercase blockName with
         | "sub" | "sublib" | "library" -> (
             let doSub = do_block "library" (make_prefix acc.name) (parse strict)
                 (fun obj -> { acc with subs = obj :: acc.subs})
             in
             doSub args cont
           )
         | "per" -> (
             let t = acc.target in
             let doPer = do_block2 "per" newTargetExtra (parse_per strict)
                 (fun obj -> { acc with target = { t with target_extras = obj :: t.target_extras } }) in
             doPer args cont
           )
         | _ -> raise_if_strict strict ("unexpected block name in library: " ^ blockName); acc
      )
    | (k, Some v) ->
      let (value: string) = String.concat "\n" (v :: List.map snd cont) in
      (match String.lowercase k with
       | "modules"     -> { acc with modules = parse_module_name value @ acc.modules }
       | "pack"        -> { acc with pack    = user_bool_of_string "pack" value }
       | "syntax"      -> { acc with syntax  = user_bool_of_string "syntax" value }
       | "description" -> { acc with description = value }
       | "sub" | "sublib" | "library" -> raise (BlockSectionAsValue k)
       | k             -> { acc with target  = parse_target strict acc.target k value }
      )
end

module Executable = struct
  type t = {
    name      : string;
    main      : filename;
    target    : target;
  }

  let make name = {
    name;
    main   = emptyFn;
    target = newTarget (Name.Exe name) Typ.Exe true true
  }

  let to_target obj = obj.target

  let parse_common strict sectionName setMain setTarget myTarget other acc line cont =
    match Utils.toKV line with
    | (k, None) ->
      (match string_words_noempty k with
       | [] -> raise_if_strict strict ("unknown empty block in " ^ sectionName ^ " " ^ k); acc
       | blockName :: args -> (
           match String.lowercase blockName with
           | "per" -> (
               let t = myTarget in
               let doPer = do_block2 "per" (newTargetExtra) (parse_per strict)
                   (fun obj -> setTarget acc { t with target_extras = obj :: t.target_extras }) in
               doPer args cont
             )
           | _                            -> raise_if_strict strict ("unexpected block name in library: " ^ blockName); acc
         )
      )
    | (k, Some v) ->
      let (value: string) = String.concat "\n" (v :: List.map snd cont) in
      (match String.lowercase k with
       | "main" | "mainis"
       | "main-is"    -> setMain acc (fn value)
       | k            ->
         try let f = List.assoc k other in f acc value
         with Not_found -> setTarget acc (parse_target strict myTarget k value)
      )

  let parse strict obj =
    parse_common strict "executable" (fun acc main -> { acc with main = main })
      (fun acc target -> { acc with target = target })
      obj.target
      []
      obj

  let find exes name = try List.find (fun e -> e.name = name) exes
    with Not_found -> raise (ExecutableNotFound name)

end

module Test = struct
  type test_type = ExitCode

  type t = {
    name     : string;
    main     : filename;
    target   : target;
    rundir   : filepath option;
    runopt   : string list;
    type_     : test_type;
  }


  let make name = {
    name;
    main   = emptyFn;
    target = newTarget (Name.Test name) Typ.Test (Gconf.get_target_option "build-tests") false;
    rundir = None;
    runopt = [];
    type_   = ExitCode;
  }

  let to_target obj = obj.target

  let parse strict obj =
    Executable.parse_common strict "test" (fun acc main -> { acc with main = main })
      (fun acc target -> { acc with target = target })
      obj.target
      [ ("rundir", (fun acc v -> { acc with rundir = Some (fp v) }))
      ; ("runopt", (fun acc v -> { acc with runopt = acc.runopt @ string_words v }))
      ]
      obj

  let find tests name = try List.find (fun b -> b.name = name) tests
    with Not_found -> raise (TestNotFound name)
end

module Bench = struct
  type t = {
    name     : string;
    main     : filename;
    target   : target;
    (* TODO add bench type *)
  }

  let to_target obj = obj.target

  let find benchs name = try List.find (fun b -> b.name = name) benchs
    with Not_found -> raise (BenchNotFound name)
end

(* an example is an executable that doesn't get installed.
 * or maybe install in a documentation directory
 *)
module Example = struct
  type t = {
    name     : string;
    main     : filename;
    target   : target;
  }

  let to_target obj = obj.target

  let make name = {
    name   = name;
    main   = emptyFn;
    target = newTarget (Name.Example name) Typ.Test (Gconf.get_target_option "build-examples") false;
  }

  let parse strict obj =
    Executable.parse_common strict "example" (fun acc main -> { acc with main = main })
      (fun acc target -> { acc with target = target })
      obj.target
      []
      obj

  let find examples name = try List.find (fun b -> b.name = name) examples
    with Not_found -> raise (ExampleNotFound name)

end

module Flag = struct
  type t = {
    name        : string;
    description : string;
    default     : bool option;
  }

  let make args = {
    name = get_context_name "flag" args;
    description = "";
    default = None;
  }

  let parse strict acc line cont =
    match Utils.toKV line with
    | (k, None)   -> raise_if_strict strict ("unexpected item in flag " ^ k); acc
    | (k, Some v) ->
      let (value: string) = String.concat "\n" (v :: List.map snd cont) in
      (match String.lowercase k with
       | "description" -> { acc with description = value }
       | "default"     -> { acc with default = Some (user_bool_of_string "flag default" value) }
       | k             -> (raise_if_strict strict ("unexpected item in flag : " ^ k); acc)
      )

  let find flags name = try Some (List.find (fun fl -> fl.name = name) flags)
    with Not_found -> None
end

type t = {
  name        : string;
  version     : string;
  synopsis    : string;
  description : string;
  license     : string;
  license_file: filepath option;
  authors     : string list;
  obuild_ver  : int;
  ocaml_ver   : Expr.t option;
  homepage    : string;
  flags       : Flag.t list;
  libs        : Library.t list;
  exes        : Executable.t list;
  tests       : Test.t list;
  benchs      : Bench.t list;
  examples    : Example.t list;
  extra_srcs  : filepath list;
  extra_tools : filename list;
  configure_script : filepath option;
  ocaml_extra_args : string list option;
}

let make = {
  name        = "";
  version     = "";
  synopsis    = "";
  description = "";
  license     = "";
  license_file= None;
  authors     = [];
  obuild_ver  = 0;
  ocaml_ver   = None;
  homepage    = "";
  extra_tools = [];
  flags       = [];
  libs        = [];
  exes        = [];
  tests       = [];
  benchs      = [];
  examples    = [];
  extra_srcs  = [];
  configure_script = None;
  ocaml_extra_args = None;
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

let parse strict lines =
  let parse_library acc = do_block "library" Library.make_from_string (Library.parse strict)
      (fun obj -> { acc with libs = obj :: acc.libs })
  in
  let parse_executable acc = do_block "executable" Executable.make (Executable.parse strict)
      (fun obj -> { acc with exes = obj :: acc.exes })
  in
  let parse_test acc = do_block "test" Test.make (Test.parse strict)
      (fun obj -> { acc with tests = obj :: acc.tests })
  in
  let parse_example acc = do_block "example" Example.make (Example.parse strict)
      (fun obj -> { acc with examples = obj :: acc.examples })
  in
  let parse_flag acc args cont =
    let flag = process_chunk (Flag.parse strict) (Flag.make args) cont in
    { acc with flags = flag :: acc.flags }
  in
    (*************    root parsing    *******************************)
  let parse_root acc (line: string) (cont: (int*string) list) =
    match Utils.toKV line with
    | (k, None) ->
      (match string_words_noempty k with
       | []                   -> raise_if_strict strict ("unknown empty block"); acc
       | blockName :: args    ->
         match String.lowercase blockName with
         | "executable" -> parse_executable acc args cont
         | "library"    -> parse_library acc args cont
         | "flag"       -> parse_flag acc args cont
         | "test"       -> parse_test acc args cont
         | "bench"      -> raise_if_strict strict ("unimplemented section: " ^ blockName); acc
         | "example"    -> parse_example acc args cont
         | _            -> raise_if_strict strict ("unknown block name: " ^ blockName); acc
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
        | "tools"       -> { acc with extra_tools = List.map fn (string_words_noempty value) @ acc.extra_tools }
        | "authors"     -> { acc with authors = Utils.parseCSV value }
        | "author"      -> { acc with authors = [value] }
        | "extra-srcs"  -> { acc with extra_srcs = List.map fp (Utils.parseCSV value) @ acc.extra_srcs }
        | "obuild-ver"  -> { acc with obuild_ver = user_int_of_string "obuild-ver" value }
        | "ocamlversion"
        | "ocaml-version" -> { acc with ocaml_ver = Expr.parse "ocaml-version" value }
        | "configure-script" -> { acc with configure_script = Some (fp value) }
        | "ocaml-extra-args" | "ocamlextraargs" ->
          let v = string_words_noempty value in
          Gconf.gconf.Gconf.ocaml_extra_args <- v;
          { acc with ocaml_extra_args = Some v }
        (* for better error reporting *)
        | "executable" | "library" | "test" | "bench" | "example" -> raise (BlockSectionAsValue k)
        | k             -> raise_if_strict strict ("unknown key: " ^ k); acc
      )
  in
  process_chunk parse_root make lines

let check proj =
    (if proj.name = "" then raise (MissingField "name"));
    (if proj.version = "" then raise (MissingField "version"));
    (if proj.obuild_ver = 0 then raise (MissingField "obuild-ver"));
    (if proj.obuild_ver > 1 then raise (UnsupportedFutureVersion proj.obuild_ver));

    let check_files_exists target names =
        let srcdir = target.target_obits.target_srcdir in
        List.iter (fun n -> ignore(Utils.find_in_paths srcdir n)) names
    in

    let check_modules_exists target modules =
        let srcdir = target.target_obits.target_srcdir in
        List.iter (fun m -> try ignore(Hier.get_file_entry m srcdir)
                    with Not_found ->
                      raise (ModuleDoesntExist (target, m))
                  ) modules
    in

    maybe_unit (fun x -> if not (Filesystem.exists x) then raise (LicenseFileDoesntExist x)) proj.license_file;
    maybe_unit (fun x -> let ocaml_ver = Hashtbl.find (Prog.getOcamlConfig ()) "version" in
                 if not (Expr.eval ocaml_ver x) then raise (BadOcamlVersion (ocaml_ver,x))) proj.ocaml_ver;

    (* check sublibs in libs *)
    List.iter (fun rootlib ->
        Library.check_modules_not_empty rootlib;
        let sublibs = Library.flatten rootlib in
        List.iter (fun lib ->
            Library.check_modules_not_empty lib;
            check_modules_exists lib.Library.target lib.Library.modules) sublibs
    ) proj.libs;

    List.iter (fun exe ->
        if fn_to_string exe.Executable.main = ""
            then raise (ExecutableWithNoMain exe.Executable.name);
        check_files_exists exe.Executable.target [exe.Executable.main]
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
        maybe () (fun x -> add_string "ocaml-version" (Expr.to_string x)) proj.ocaml_ver;
        maybe () (fun x -> add_string "ocaml-extra-args" (String.concat " " x)) proj.ocaml_extra_args;

        let show_target iStr target =
            let obits = target.target_obits in
            let cbits = target.target_cbits in
            add (sprintf "%ssrc-dir: %s\n" iStr (String.concat "," (List.map fp_to_string obits.target_srcdir)));
            add_string (iStr ^ "build-deps") (Utils.showList ", " (fun (l,_) -> Libname.to_string l) obits.target_builddeps);
            add_string (iStr ^ "oflags") (Utils.showList " " id obits.target_oflags);
            add_string (iStr ^ "pp") (maybe "" (fun ppty -> Pp.Type.to_string ppty) obits.target_pp);

            add (sprintf "%sc-dir: %s\n" iStr (fp_to_string cbits.target_cdir));
            add_string (iStr ^ "c-sources") (Utils.showList ", " fn_to_string cbits.target_csources);
            add_string (iStr ^ "c-flags") (Utils.showList " " id cbits.target_cflags);
            add_string (iStr ^ "c-libs") (Utils.showList "," id cbits.target_clibs);
            add_string (iStr ^ "c-libpaths") (Utils.showList "," fp_to_string cbits.target_clibpaths);
            add_string (iStr ^ "c-pkgs") (Utils.showList ", " (fun (l,_) -> l) cbits.target_cpkgs);
            in
        List.iter (Library.show add show_target "") proj.libs;
        List.iter (fun exe ->
            add "\n";
            add (sprintf "executable %s\n" exe.Executable.name);
            add (sprintf "  main: %s\n" (fn_to_string exe.Executable.main));
            show_target "  " exe.Executable.target;
            ()
        ) proj.exes;
    )

let get_all_targets projFile =
      List.concat (List.map Library.to_targets projFile.libs)
    @ List.map Executable.to_target projFile.exes
    @ List.map Test.to_target projFile.tests
    @ List.map Bench.to_target projFile.benchs
    @ List.map Example.to_target projFile.examples

let get_all_targets_filter projFile f =
  List.filter (fun target -> f target) (get_all_targets projFile)

let get_val_const_or_var user_flags = function
  | BoolConst t    -> t
  | BoolVariable v ->
    try List.assoc v user_flags
    with Not_found -> raise (UnknownFlag v)

let get_all_buildable_targets proj_file user_flags =
  get_all_targets_filter proj_file (fun target -> get_val_const_or_var user_flags target.target_buildable)

let get_all_installable_targets proj_file user_flags =
  get_all_targets_filter proj_file (fun target ->
      let install = get_val_const_or_var user_flags target.target_installable in
      let build = get_val_const_or_var user_flags target.target_buildable in
      Printf.printf "target %s install %b build %b\n" (Target.Name.to_string target.target_name) install build;
      install)


let find_lib proj_file name = Library.find proj_file.libs name
let find_exe proj_file name = Executable.find proj_file.exes name
let find_test proj_file name = Test.find proj_file.tests name
let find_bench proj_file name = Bench.find proj_file.benchs name
let find_example proj_file name = Example.find proj_file.examples name
let find_flag name proj_file = Flag.find proj_file.flags name

