open Ext.Filepath
open Types

type t = string

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

let wrap x =
    if String.length x = 0 then (raise EmptyModuleName)
    else if not (string_all char_is_valid_modchar x) then (raise (InvalidModuleName x))
    else if Char.uppercase x.[0] <> x.[0] then (raise (InvalidModuleName x))
    else x

let of_string x = wrap x
let to_string x = x
let to_dir x = String.uncapitalize x

let to_libstring lib = String.concat "_" (lib_name_to_string_nodes lib)
let cmxs_of_lib (compileType: ocaml_compilation_option) lib = fn (to_libstring lib ^ extDP compileType ^ ".cmxs")
let cmxa_of_lib (compileType: ocaml_compilation_option) lib = fn (to_libstring lib ^ extDP compileType ^ ".cmxa")
let cma_of_lib (compileType: ocaml_compilation_option) lib = fn (to_libstring lib ^ extDP compileType ^ ".cma")
let cmca_of_lib b = if b = Native then cmxa_of_lib else cma_of_lib

(* only used for stdlib stuff *)
let lib_of_cmca b file =
  let suffix = if b = Native then ".cmxa" else ".cma" in
  Filename.chop_suffix (fn_to_string file) suffix

let to_x ext modname = fn (String.uncapitalize modname ^ ext)
let to_o = to_x ".o"
let to_directory = to_x ""
let to_filename = to_x ".ml"
let to_parser = to_x ".mly"
let to_lexer = to_x ".mll"

let module_lookup_methods = [ to_directory; to_parser; to_lexer; to_filename ]

let of_directory filename = wrap (String.capitalize (fn_to_string filename))
let of_filename filename =
  try wrap (String.capitalize (Filename.chop_extension (fn_to_string filename)))
  with EmptyModuleName -> raise (ModuleFilenameNotValid (fn_to_string filename))
     | Invalid_argument _ -> raise (ModuleFilenameNotValid (fn_to_string filename))
