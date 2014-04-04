open Ext.Fugue
open Ext.Filepath
open Ext
open Gconf

exception OCamlProgramError of string
exception TarError of string
exception PkgConfigError of string
exception PkgConfigErrorNoVersion
exception PkgConfigErrorUnexpectedOutput of string
exception ProgramNotFound of string

let prog_cache = Hashtbl.create 64
let getCache prog getNames =
    try Hashtbl.find prog_cache prog
    with Not_found ->
        let names = getNames () in
        try
            let syspath = Utils.get_system_paths () in
            list_findmap (fun n ->
                if Filename.is_implicit n
                    then (try
                            let foundPath = Utils.find_in_paths syspath (fn n) in
                            Some (fp_to_string (foundPath </> fn n))
                         with Utils.FileNotFoundInPaths _ -> None)
                    else (if Filesystem.exists (fp n) then Some n else None)
            ) names
        with Not_found ->
            raise (ProgramNotFound prog)
        

let getOcamlOpt () = getCache "ocamlopt"   (fun n -> maybe ["ocamlopt.opt"; "ocamlopt"] list_singleton gconf.conf_prog_ocamlopt)
let getOcamlC   () = getCache "ocamlc"     (fun n -> maybe ["ocamlc.opt"; "ocamlc"] list_singleton gconf.conf_prog_ocamlc)
let getOcamlDep () = getCache "ocamldep"   (fun n -> maybe ["ocamldep.opt"; "ocamldep"] list_singleton gconf.conf_prog_ocamldep)
let getOcamlDoc () = getCache "ocamldoc"   (fun n -> maybe ["ocamldoc.opt"; "ocamldoc"] list_singleton gconf.conf_prog_ocamldoc)
let getOcamlYacc ()= getCache "ocamlyacc"  (fun n -> maybe ["ocamlyacc"] list_singleton gconf.conf_prog_ocamlyacc)
let getOcamlLex () = getCache "ocamllex"   (fun n -> maybe ["ocamllex.opt"; "ocamllex"] list_singleton gconf.conf_prog_ocamllex)
let getOcamlMklib () = getCache "ocamlmklib" (fun n -> maybe ["ocamlmklib"] list_singleton gconf.conf_prog_ocamlmklib)
let getCamlp4   () = getCache "camlp4"     (fun n -> maybe ["camlp4"] list_singleton gconf.conf_prog_camlp4)
let getCC       () = getCache "cc"         (fun n -> [ default "gcc" gconf.conf_prog_cc])
let getRanlib   () = getCache "ranlib"     (fun n -> [ default "ranlib" gconf.conf_prog_ranlib])
let getAR       () = getCache "ar"         (fun n -> [ default "ar" gconf.conf_prog_ar])
let getLD       () = getCache "ld"         (fun n -> [ default "ld" gconf.conf_prog_ld])
let getPkgConfig() = getCache "pkg-config" (fun n -> [ default "pkg-config" gconf.conf_prog_pkgconfig])

let getOcamlVersion () =
    match Process.run [ getOcamlOpt (); "-vnum" ] with
    | Process.Success (s,_,_) -> (match string_split ~limit:3 '.' s with
                   | [major;minor;other] ->
                           (user_int_of_string "ocaml version major" major
                           ,user_int_of_string "ocaml version minor" minor
                           ,other
                           )
                   | _ -> raise (OCamlProgramError ("ocaml return an unknown version " ^ s))
                   )
    | Process.Failure err -> raise (OCamlProgramError err)

let getOcamlConfig () =
    match Process.run [ getOcamlOpt (); "-config" ] with
    | Process.Success (s,_,_) ->
        let lines = string_lines_noempty s in
        let h = Hashtbl.create 32 in
        List.iter (fun l ->
            let (k,v) = Utils.toKV l in
            Hashtbl.add h k (default "" v)
        ) lines;
        h
    | Process.Failure err -> raise (OCamlProgramError ("ocamlopt cannot get config " ^ err))

let getCamlp4Config () =
    match Process.run [ getCamlp4 (); "-where" ] with
    | Process.Success (s,_,_) ->
        let (l:_) = string_lines_noempty s in
        l
    | Process.Failure err -> raise (OCamlProgramError ("ocamlopt cannot get config " ^ err))
 
let runTar output dir =
    match Process.run [ "tar"; "czf"; output; dir ] with
    | Process.Success _   -> ()
    | Process.Failure err -> raise (TarError err)

let runPkgConfig typ name =
    match Process.run [ getPkgConfig (); typ; name ] with
    | Process.Success (s,_,_) -> s
    | Process.Failure err   -> raise (PkgConfigError err)

let runPkgConfigVersion name =
    let output = runPkgConfig "--version" name in
    match string_words_noempty output with
    | [ver] -> ver
    | []    -> raise PkgConfigErrorNoVersion
    | _     -> raise (PkgConfigErrorUnexpectedOutput ("version: " ^ output))

let runPkgConfigIncludes name =
    let output = runPkgConfig "--cflags" name in
    (* FIXME check if every items actually got -L as expected *)
    List.map (string_drop 2) (string_words_noempty output)

let runPkgConfigLibs name =
    let output = runPkgConfig "--libs" name in
    (* FIXME check if every items actually got -l as expected *)
    List.map (string_drop 2) (string_words_noempty output)
