open Ext.Filepath
open Types

type modname = { _modname : string }

exception InvalidModuleName of string
exception EmptyModuleName
exception ModuleFilenameNotValid of string

let char_isalpha c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let char_is_valid_modchar c =
    char_isalpha c || (c >= '0' && c <= '9') || c == '_'
 
let string_all p s =
    let valid = ref true in
    for i = 0 to String.length s - 1 do valid := !valid && p s.[i] done;
    !valid

let wrap_module x =
    if String.length x = 0 then (raise EmptyModuleName)
    else if not (string_all char_is_valid_modchar x) then (raise (InvalidModuleName x))
    else if Char.uppercase x.[0] <> x.[0] then (raise (InvalidModuleName x))
    else { _modname = x }

let modname_of_string x = wrap_module x
let modname_to_string x = x._modname
let modname_to_dir x = String.uncapitalize x._modname

let to_libstring lib = String.concat "_" (lib_name_to_string_nodes lib)
let cmxa_of_lib (compileType: ocaml_compilation_option) lib = fn (to_libstring lib ^ extDP compileType ^ ".cmxa")
let cma_of_lib (compileType: ocaml_compilation_option) lib = fn (to_libstring lib ^ extDP compileType ^ ".cma")
let cmca_of_lib b = if b = Native then cmxa_of_lib else cma_of_lib

(* only used for stdlib stuff *)
let lib_of_cmca b file =
    let suffix = if b = Native then ".cmxa" else ".cma" in
    Filename.chop_suffix (fn_to_string file) suffix

let o_of_module modname = fn (String.uncapitalize modname._modname ^ ".o")

let directory_of_module modname = fn (String.uncapitalize modname._modname)
let filename_of_module modname = fn (String.uncapitalize modname._modname ^ ".ml")
let parser_of_module modname = fn (String.uncapitalize modname._modname ^ ".mly")
let lexer_of_module modname = fn (String.uncapitalize modname._modname ^ ".mll")

let module_lookup_methods = [ directory_of_module; parser_of_module; lexer_of_module; filename_of_module ]

let module_of_directory filename = wrap_module (String.capitalize (fn_to_string filename))
let module_of_parser filename = wrap_module (String.capitalize (Filename.chop_extension (fn_to_string filename)))
let module_of_lexer filename = wrap_module (String.capitalize (Filename.chop_extension (fn_to_string filename)))
let module_of_filename filename =
    try wrap_module (String.capitalize (Filename.chop_extension (fn_to_string filename)))
    with EmptyModuleName -> raise (ModuleFilenameNotValid (fn_to_string filename))
       | Invalid_argument _ -> raise (ModuleFilenameNotValid (fn_to_string filename))
