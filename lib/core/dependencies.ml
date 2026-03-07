open Filepath
open Compat

exception BuildDepAnalyzeFailed of string
exception BuildCDepAnalyzeFailed of string
exception DependencyMissing of string
exception DependenciesMissing of string list
exception DependencyFailedParsing of string

type dependency = Libname.t * Expr.t option
type cdependency = string * Expr.t option

type dep_opt = {
  dep_includes : filepath list;
  dep_pp : Pp.t;
}

let parse_output_KsemiVs onNonKV mapFstTy mapSndTys out =
  List.map
    (fun (k, mv) ->
      match mv with
      | None -> onNonKV k
      | Some v -> (mapFstTy k, List.map mapSndTys (String_utils.words_noempty v)))
    (List.map Utils.toKV (String_utils.lines_noempty out))

(* return the (modules list) dependency for a specific file *)
let run_ocamldep dopt srcFile =
  let wrap_module_safe f =
    try Modname.wrap f
    with Modname.InvalidModuleName _ | Modname.EmptyModuleName ->
      raise (BuildDepAnalyzeFailed ("ocamldep returned a bad module name " ^ f))
  in
  let fileType = Filetype.of_filepath srcFile in
  let baseFile = fp_to_string srcFile in
  let files =
    if fileType = Filetype.FileML then
      [ baseFile; baseFile ^ "i" ]
    else
      [ baseFile ]
  in
  let args =
    [ Prog.get_ocamldep () ]
    @ Utils.to_include_path_options dopt.dep_includes
    @ Pp.to_params dopt.dep_pp @ [ "-modules" ] @ files
  in
  match Process.run args with
  | Process.Failure (_, er, _) -> raise (BuildDepAnalyzeFailed er)
  | Process.Success (out, _, _) ->
      List.map snd
        (parse_output_KsemiVs
           (fun _ -> raise (BuildDepAnalyzeFailed ("assumption failed: " ^ out)))
           fp wrap_module_safe out)

(* Batch-run ocamldep on a list of .ml source files sharing the same dep_opt.
 * Fills `cache` with (fp_to_string srcFile -> merged Modname.t list) entries.
 * Silently skips on failure — callers fall back to run_ocamldep per-file. *)
let fill_ocamldep_cache cache dopt ml_files =
  if ml_files = [] then ()
  else
    let wrap_safe f =
      try Some (Modname.wrap f)
      with Modname.InvalidModuleName _ | Modname.EmptyModuleName -> None
    in
    let all_args =
      List.concat (List.map (fun f ->
        let base = fp_to_string f in
        let mli = base ^ "i" in
        base :: (if Filesystem.exists (fp mli) then [ mli ] else [])
      ) ml_files)
    in
    let args =
      [ Prog.get_ocamldep () ]
      @ Utils.to_include_path_options dopt.dep_includes
      @ Pp.to_params dopt.dep_pp @ [ "-modules" ] @ all_args
    in
    (match Process.run args with
    | Process.Failure _ -> ()
    | Process.Success (out, _, _) ->
        let file_to_deps : (string, Modname.t list) Hashtbl.t =
          Hashtbl.create (List.length ml_files * 2)
        in
        List.iter
          (fun line ->
            match Utils.toKV line with
            | _, None -> ()
            | k, Some v ->
                let deps = SafeList.filter_map wrap_safe (String_utils.words_noempty v) in
                Hashtbl.replace file_to_deps k deps)
          (String_utils.lines_noempty out);
        List.iter
          (fun f ->
            let base = fp_to_string f in
            if Hashtbl.mem file_to_deps base || Hashtbl.mem file_to_deps (base ^ "i") then begin
              let ml_deps  = (try Hashtbl.find file_to_deps base       with Not_found -> []) in
              let mli_deps = (try Hashtbl.find file_to_deps (base ^ "i") with Not_found -> []) in
              let seen = Hashtbl.create 16 in
              let merged = List.filter (fun x ->
                if Hashtbl.mem seen x then false
                else (Hashtbl.replace seen x (); true)
              ) (ml_deps @ mli_deps) in
              Hashtbl.replace cache base merged
            end)
          ml_files)

(* TODO
 * gcc escape spaces in filename with a \, tweak strings_words_noempty
 * to take that in consideration.
 *)
let joinLines s =
  let s = bytes_of_string s in
  let s_end = bytes_length s in
  let rec replace start =
    try
      let index = bytes_index_from s start '\\' in
      if index < s_end - 1 then
        if bytes_get s (index + 1) = '\n' then begin
          bytes_set s index ' ';
          bytes_set s (index + 1) ' ';
          replace (index + 2)
        end
        else
          replace (index + 1)
      else
        s
    with Not_found -> s
  in
  bytes_to_string (replace 0)

let run_ccdep srcDir files : (filename * filepath list) list =
  let args = [ Prog.get_cc (); "-MM" ] @ List.map (fun fn -> fp_to_string (srcDir </> fn)) files in
  match Process.run args with
  | Process.Failure (_, err, _) -> raise (BuildCDepAnalyzeFailed err)
  | Process.Success (out, _, _) ->
      parse_output_KsemiVs
        (fun _ -> raise (BuildCDepAnalyzeFailed "missing semicolon in gcc dependency output"))
        fn fp (joinLines out)
