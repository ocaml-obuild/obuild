open Ext.Fugue

exception EmptyLibName

type flag_tweak = SetFlag of string | ClearFlag of string

type flag_name = string

type exe_name = string

(* represent a library in a form abc[.def.xyz] *)
type lib_name = { lib_main_name : string; lib_subnames : string list } 

let lib_name_of_string s =
    match string_split '.' s with
    | []    -> raise EmptyLibName
    | x::xs -> { lib_main_name = x; lib_subnames = xs }

let lib_name_to_string lname = String.concat "." (lname.lib_main_name :: lname.lib_subnames)
let lib_name_to_string_nodes lname = lname.lib_main_name :: lname.lib_subnames

let lib_name_append lname sub = { lname with lib_subnames = lname.lib_subnames @ [sub] }

type name =
      LibName of lib_name
    | ExeName of exe_name
    | TestName of exe_name
    | BenchName of exe_name
    | ExampleName of exe_name

let name_to_string name =
    match name with
    | ExeName e   -> "exe-" ^ e
    | BenchName e -> "bench-" ^ e
    | TestName e  -> "test-" ^ e
    | ExampleName e -> "example-" ^ e
    | LibName l   -> "lib-" ^ lib_name_to_string l

type ocaml_compilation_option = Normal | WithDebug | WithProf
type ocaml_compiled_type = ByteCode | Native
type ocaml_compilation_mode = Interface | Compiled of ocaml_compiled_type

let extDP compileType =
    match compileType with
    | Normal    -> ""
    | WithDebug -> ".d"
    | WithProf  -> ".p"
