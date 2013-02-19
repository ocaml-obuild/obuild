open Types
open Ext
open Modname
open Filepath
open Helper
open Pp

exception BuildDepAnalyzeFailed of string
exception BuildCDepAnalyzeFailed of string

exception DependencyMissing of string
exception DependencyFailedParsing of string

type dep_constraint = Expr.expr

type dep_main_name = string

(* represent a dependency in a form abc[.def.xyz] *)
type dep_name = lib_name

type dependency = dep_name * (dep_constraint option)

type c_dep_name = string

type cdependency = c_dep_name * (dep_constraint option)

type dep_opt =
    { dep_includes: filepath list
    ; dep_pp      : pp
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
        try wrap_module f
        with _ -> raise (BuildDepAnalyzeFailed ("ocamldep returned a bad module name " ^ f))
        in
    let args = [Prog.getOcamlDep ()]
             @ (Utils.to_include_path_options dopt.dep_includes)
             @ (Pp.pp_to_params dopt.dep_pp)
             @ ["-modules"; fp_to_string srcFile ] in
    match Process.run_with_outputs args with
    | Process.Failure er -> raise (BuildDepAnalyzeFailed er)
    | Process.Success (out,_) ->
        List.map snd (parse_output_KsemiVs
            (fun _ -> raise (BuildDepAnalyzeFailed ("assumption failed: " ^ out)))
            fp wrap_module_safe out
        )

(* TODO
 * gcc escape spaces in filename with a \, tweak strings_words_noempty
 * to take that in consideration.
 *)
let runCCdep srcDir files : (filename * filepath list) list =
    let args = [Prog.getCC (); "-MM"] @ List.map (fun fn -> fp_to_string (srcDir </> fn)) files in
    match Process.run_with_outputs args with
    | Process.Failure err     -> raise (BuildCDepAnalyzeFailed err)
    | Process.Success (out,_) ->
        parse_output_KsemiVs
            (fun _ -> raise (BuildCDepAnalyzeFailed "missing semicolon in gcc dependency output"))
            fn fp out

