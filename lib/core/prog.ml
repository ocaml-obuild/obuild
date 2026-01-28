open Fugue
open Filepath

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
  | None -> (
      try
        let syspath = Utils.get_system_paths () in
        let found =
          list_find_map
            (fun n ->
              let n = if Utils.isWindows then n ^ ".exe" else n in
              if Filename.is_implicit n then
                try
                  let found_path = Utils.find_in_paths syspath (fn n) in
                  Some (fp_to_string (found_path </> fn n))
                with Utils.FileNotFoundInPaths _ -> None
              else if Filesystem.exists (fp n) then
                Some n
              else
                None)
            names
        in
        Gconf.set_env prog found;
        found
      with Not_found -> raise (ProgramNotFound prog))

let get_ocamlopt () = get_cache "ocamlopt" [ "ocamlopt.opt"; "ocamlopt" ]
let get_ocamlc () = get_cache "ocamlc" [ "ocamlc.opt"; "ocamlc" ]
let get_ocamldep () = get_cache "ocamldep" [ "ocamldep.opt"; "ocamldep" ]
let get_ocamldoc () = get_cache "ocamldoc" [ "ocamldoc.opt"; "ocamldoc" ]
let get_ocamlmklib () = get_cache "ocamlmklib" [ "ocamlmklib" ]
let get_camlp4 () = get_cache "camlp4" [ "camlp4" ]
let get_cc () = get_cache "cc" [ "gcc" ]
let get_ranlib () = get_cache "ranlib" [ "ranlib" ]
let get_ar () = get_cache "ar" [ "ar" ]
let get_ld () = get_cache "ld" [ "ld" ]
let get_pkg_config () = get_cache "pkg-config" [ "pkg-config" ]
let get_ocaml () = get_cache "ocaml" [ "ocaml" ]
let get_ocamlmktop () = get_cache "ocamlmktop" [ "ocamlmktop" ]

let get_ocaml_version cfg =
  let ver = Hashtbl.find cfg "version" in
  match String_utils.split ~limit:3 '.' ver with
  | [ major; minor; other ] -> (major, minor, other)
  | _ -> raise (OCamlProgramError ("ocaml return an unknown version " ^ ver))

let ocaml_config = ref None

let get_ocaml_config () =
  match !ocaml_config with
  | None -> (
      match Process.run [ get_ocamlc (); "-config" ] with
      | Process.Success (s, _, _) ->
          let lines = String_utils.lines_noempty s in
          let h = Hashtbl.create 32 in
          List.iter
            (fun l ->
              let k, v = Utils.toKV l in
              Hashtbl.add h k (default "" v))
            lines;
          ocaml_config := Some h;
          h
      | Process.Failure err -> raise (OCamlProgramError ("ocamlc cannot get config " ^ err)))
  | Some h -> h

let get_camlp4_config () =
  match Process.run [ get_camlp4 (); "-where" ] with
  | Process.Success (s, _, _) ->
      let (l : _) = String_utils.lines_noempty s in
      l
  | Process.Failure err -> raise (OCamlProgramError ("ocamlopt cannot get config " ^ err))

let run_tar output dir =
  match Process.run [ "tar"; "czf"; output; dir ] with
  | Process.Success _ -> ()
  | Process.Failure err -> raise (TarError err)

let run_pkg_config typ name =
  match Process.run [ get_pkg_config (); typ; name ] with
  | Process.Success (s, _, _) -> s
  | Process.Failure err -> raise (PkgConfigError err)

let run_pkg_config_version name =
  let output = run_pkg_config "--version" name in
  match String_utils.words_noempty output with
  | [ ver ] -> ver
  | [] -> raise PkgConfigErrorNoVersion
  | _ -> raise (PkgConfigErrorUnexpectedOutput ("version: " ^ output))

let run_pkg_config_includes name =
  let output = run_pkg_config "--cflags" name in
  (* FIXME check if every items actually got -L as expected *)
  List.map (String_utils.drop 2) (String_utils.words_noempty output)

let run_pkg_config_libs name =
  let output = run_pkg_config "--libs" name in
  (* FIXME check if every items actually got -l as expected *)
  List.map (String_utils.drop 2) (String_utils.words_noempty output)
