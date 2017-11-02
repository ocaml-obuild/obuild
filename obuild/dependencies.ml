open Ext.Fugue
open Ext.Filepath
open Ext.Compat
       
exception BuildDepAnalyzeFailed of string
exception BuildCDepAnalyzeFailed of string

exception DependencyMissing of string
exception DependenciesMissing of string list
exception DependencyFailedParsing of string

type dependency = Libname.t * (Expr.t option)

type cdependency = string * (Expr.t option)

type dep_opt =
    { dep_includes: filepath list
    ; dep_pp      : Pp.t
    }

let parse_output_KsemiVs onNonKV mapFstTy mapSndTys out =
    List.map (fun (k, mv) ->
        match mv with
        | None   -> onNonKV k
        | Some v -> (mapFstTy k, List.map mapSndTys (string_words_noempty v))
    ) (List.map Utils.toKV (string_lines_noempty out))

(* return the (modules list) dependency for a specific file *)
let runOcamldep dopt srcFile =
  let wrap_module_safe f =
    try Modname.wrap f
    with _ -> raise (BuildDepAnalyzeFailed ("ocamldep returned a bad module name " ^ f))
  in
  let fileType = Filetype.of_filepath srcFile in
  let baseFile = fp_to_string srcFile in
  let files = if fileType = Filetype.FileML then [baseFile; baseFile ^ "i"]
    else [baseFile] in
  let args = [Prog.getOcamlDep ()]
             @ (Utils.to_include_path_options dopt.dep_includes)
             @ (Pp.to_params dopt.dep_pp)
             @ ["-modules"] @ files in
  match Process.run args with
  | Process.Failure er -> raise (BuildDepAnalyzeFailed er)
  | Process.Success (out,_,_) ->
    List.map snd (parse_output_KsemiVs
                    (fun _ -> raise (BuildDepAnalyzeFailed ("assumption failed: " ^ out)))
                    fp wrap_module_safe out
                 )

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
        if (bytes_get s (index + 1)) = '\n' then begin
          bytes_set s index  ' ';
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

let runCCdep srcDir files : (filename * filepath list) list =
  let args = [Prog.getCC (); "-MM"] @ List.map (fun fn -> fp_to_string (srcDir </> fn)) files in
  match Process.run args with
  | Process.Failure err     -> raise (BuildCDepAnalyzeFailed err)
  | Process.Success (out,_,_) ->
    parse_output_KsemiVs
      (fun _ -> raise (BuildCDepAnalyzeFailed "missing semicolon in gcc dependency output"))
      fn fp (joinLines out)

