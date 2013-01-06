type verbosity = Silent | Report | Verbose | Debug | DebugPlus

type gconf =
    { mutable conf_verbosity : verbosity
    ; mutable conf_withopt   : bool
    ; mutable conf_strict    : bool
    ; mutable conf_prog_ocamlopt  : string option
    ; mutable conf_prog_ocamlc    : string option
    ; mutable conf_prog_ocamldep  : string option
    ; mutable conf_prog_cc     : string option
    ; mutable conf_prog_ranlib : string option
    ; mutable conf_prog_ar     : string option
    ; mutable conf_prog_ld     : string option
    }

let gconf =
    { conf_verbosity     = Report
    ; conf_withopt       = true
    ; conf_strict        = false
    ; conf_prog_ocamlopt = None
    ; conf_prog_ocamlc   = None
    ; conf_prog_ocamldep = None
    ; conf_prog_cc       = None
    ; conf_prog_ranlib   = None
    ; conf_prog_ar       = None
    ; conf_prog_ld       = None
    }

