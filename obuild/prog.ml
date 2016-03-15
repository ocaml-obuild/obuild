open Ext.Fugue
open Ext.Filepath
open Ext

exception OCamlProgramError of string
exception TarError of string
exception PkgConfigError of string
exception PkgConfigErrorNoVersion
exception PkgConfigErrorUnexpectedOutput of string
exception ProgramNotFound of string

let get_cache prog names =
  let res = Gconf.get_env prog in
  match res with
  | Some p -> p
  | None ->
    try
      let syspath = Utils.get_system_paths () in
      let found = list_findmap (fun n ->
          let n = if Utils.isWindows then (n ^ ".exe") else n in
          if Filename.is_implicit n
          then (try
                  let found_path = Utils.find_in_paths syspath (fn n) in
                  Some (fp_to_string (found_path </> fn n))
                with Utils.FileNotFoundInPaths _ -> None)
          else (if Filesystem.exists (fp n) then Some n else None)
        ) names
      in
      Gconf.set_env prog found;
      found
    with Not_found ->
      raise (ProgramNotFound prog)
        

let getOcamlOpt () = get_cache "ocamlopt" ["ocamlopt.opt"; "ocamlopt"]
let getOcamlC   () = get_cache "ocamlc" ["ocamlc.opt"; "ocamlc"]
let getOcamlDep () = get_cache "ocamldep" ["ocamldep.opt"; "ocamldep"]
let getOcamlDoc () = get_cache "ocamldoc" ["ocamldoc.opt"; "ocamldoc"]
let getOcamlYacc ()= get_cache "ocamlyacc" ["ocamlyacc"]
let getOcamlLex () = get_cache "ocamllex" ["ocamllex.opt"; "ocamllex"]
let getOcamlMklib () = get_cache "ocamlmklib" ["ocamlmklib"]
let getCamlp4   () = get_cache "camlp4" ["camlp4"]
let getCC       () = get_cache "cc" ["gcc"]
let getRanlib   () = get_cache "ranlib" ["ranlib"]
let getAR       () = get_cache "ar" ["ar"]
let getLD       () = get_cache "ld" ["ld"]
let getPkgConfig() = get_cache "pkg-config" ["pkg-config"]
let getOcaml () = get_cache "ocaml" ["ocaml"]
let getOcamlMktop () = get_cache "ocamlmktop" ["ocamlmktop"]
let getAtdGen () = get_cache "atdgen" ["atdgen"; "atdgen.run"]

let get_ocaml_version cfg =
  let ver = Hashtbl.find cfg "version" in
  match string_split ~limit:3 '.' ver with
  | [major;minor;other] -> (major,minor,other)
  | _ -> raise (OCamlProgramError ("ocaml return an unknown version " ^ ver))

let ocaml_config = ref None

let getOcamlConfig () =
  match !ocaml_config with
  | None ->
    (match Process.run [ getOcamlOpt (); "-config" ] with
     | Process.Success (s,_,_) ->
       let lines = string_lines_noempty s in
       let h = Hashtbl.create 32 in
       List.iter (fun l ->
           let (k,v) = Utils.toKV l in
           Hashtbl.add h k (default "" v)
         ) lines;
       ocaml_config := Some h;
       h
     | Process.Failure err -> raise (OCamlProgramError ("ocamlopt cannot get config " ^ err)))
  | Some h -> h

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
