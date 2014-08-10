open Ext.Fugue

type ocaml_compilation_option = Normal | WithDebug | WithProf
type ocaml_compiled_type = ByteCode | Native
type ocaml_compilation_mode = Interface | Compiled of ocaml_compiled_type

let extDP compileType =
    match compileType with
    | Normal    -> ""
    | WithDebug -> ".d"
    | WithProf  -> ".p"
