open Filepath

type modname = { modname : string }

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
    else { modname = x }

let modname_to_string x = x.modname

let cmxa_of_lib lib = fn (lib ^ ".cmxa")
let cma_of_lib lib = fn (lib ^ ".cma")

let extDP useDebug useProf =
    (if useDebug then ".d" else "") ^ (if useProf then ".p" else "")

let cmx_of_module useDebug useProf modname = fn (String.uncapitalize modname.modname ^ extDP useDebug useProf ^ ".cmx")
let cmo_of_module useDebug useProf modname = fn (String.uncapitalize modname.modname ^ extDP useDebug useProf ^ ".cmo")
let cmc_of_module b = if b then cmx_of_module else cmo_of_module
let cmi_of_module modname = fn (String.uncapitalize modname.modname ^ ".cmi")
let o_of_module modname = fn (String.uncapitalize modname.modname ^ ".o")

let filename_of_module modname = fn (String.uncapitalize modname.modname ^ ".ml")
let interface_of_module modname = fn (String.uncapitalize modname.modname ^ ".mli")

let module_of_filename filename =
    try wrap_module (String.capitalize (Filename.chop_extension filename.filename))
    with Invalid_argument _ -> raise (ModuleFilenameNotValid filename.filename)
