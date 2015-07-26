open Ext.Filepath
open Ext.Fugue

type t = string

exception InvalidModuleName of string
exception EmptyModuleName
exception ModuleFilenameNotValid of string

let char_isalpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let char_is_valid_modchar c = char_isalpha c || (c >= '0' && c <= '9') || c == '_'
 
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
let to_x ext modname = fn (String.uncapitalize modname ^ ext)
let to_o = to_x ".o"
let to_directory = to_x ""
let to_filename = to_x ".ml"
let to_parser = to_x ".mly"
let to_lexer = to_x ".mll"
let atd_modname modname =
  if (String.length modname) > 2 then
    let (b,e) = string_splitAt ((String.length modname) - 2) modname in
    match e with
    | "_t" | "_v" | "_j" -> b
    | _ -> modname
  else
    modname

let to_atd modname = to_x ".atd" (atd_modname modname)

let module_lookup_methods = [ to_directory; to_parser; to_lexer; to_atd; to_filename ]

let of_directory filename = wrap (String.capitalize (fn_to_string filename))
let of_filename filename =
  try wrap (String.capitalize (Filename.chop_extension (fn_to_string filename)))
  with EmptyModuleName -> raise (ModuleFilenameNotValid (fn_to_string filename))
     | Invalid_argument _ -> raise (ModuleFilenameNotValid (fn_to_string filename))
