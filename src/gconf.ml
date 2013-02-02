type verbosity = Silent | Report | Verbose | Debug | DebugPlus

type gconf =
    { mutable conf_verbosity : verbosity
    ; mutable conf_withopt   : bool
    ; mutable conf_strict    : bool
    ; mutable conf_prog_ocamlopt  : string option
    ; mutable conf_prog_ocamlc    : string option
    ; mutable conf_prog_ocamldep  : string option
    ; mutable conf_prog_ocamldoc  : string option
    ; mutable conf_prog_ocamlyacc : string option
    ; mutable conf_prog_ocamllex  : string option
    ; mutable conf_prog_cc     : string option
    ; mutable conf_prog_ranlib : string option
    ; mutable conf_prog_ar     : string option
    ; mutable conf_prog_ld     : string option
    ; mutable conf_prog_pkgconfig : string option
    ; mutable conf_prog_camlp4 : string option
    ; mutable conf_findlib_path : string option
    ; mutable conf_library_debugging : bool
    ; mutable conf_executable_debugging : bool
    ; mutable conf_library_profiling : bool
    ; mutable conf_executable_profiling : bool
    ; mutable conf_executable_as_obj : bool
    ; mutable conf_library_native : bool
    ; mutable conf_library_bytecode : bool
    ; mutable conf_executable_native : bool
    ; mutable conf_executable_bytecode : bool
    ; mutable conf_parallel_jobs : int
    ; mutable conf_dump_dot : bool
    ; mutable conf_color : bool
    ; mutable conf_build_examples : bool
    ; mutable conf_build_tests    : bool
    ; mutable conf_build_benchs   : bool
    ; mutable conf_user_flags : (string * bool) list (* TODO moved not as global variable *)
    ; mutable conf_annot : bool
    ; mutable conf_bin_annot : bool
    }

let gconf =
    { conf_verbosity     = Report
    ; conf_withopt       = true
    ; conf_strict        = false
    ; conf_prog_ocamlopt = None
    ; conf_prog_ocamlc   = None
    ; conf_prog_ocamldep = None
    ; conf_prog_ocamldoc = None
    ; conf_prog_ocamlyacc = None
    ; conf_prog_ocamllex = None
    ; conf_prog_cc       = None
    ; conf_prog_ranlib   = None
    ; conf_prog_ar       = None
    ; conf_prog_ld       = None
    ; conf_prog_pkgconfig= None
    ; conf_prog_camlp4   = None
    ; conf_findlib_path  = None
    ; conf_library_debugging    = false
    ; conf_library_profiling    = false
    ; conf_library_native       = true
    ; conf_library_bytecode     = true
    ; conf_executable_debugging = false
    ; conf_executable_profiling = false
    ; conf_executable_native    = true
    ; conf_executable_bytecode  = false
    ; conf_executable_as_obj    = false
    ; conf_parallel_jobs        = 2
    ; conf_dump_dot             = false
    ; conf_color                = false
    ; conf_build_examples       = false
    ; conf_build_tests          = false
    ; conf_build_benchs         = false
    ; conf_user_flags           = []
    ; conf_annot                = false
    ; conf_bin_annot            = false
    }
