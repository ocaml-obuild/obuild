type modname = { modname : string }
type filepath = { filepath : string }
type filename = { filename : string }

let modname_to_string x = x.modname
let filepath_to_string x = x.filepath
let filename_to_string x = x.filename

type flag_tweak = SetFlag of string | ClearFlag of string


type dep_expr = Eq of string
              | Gt of string
              | Ge of string
              | Le of string
              | Lt of string
              | And of (dep_expr * dep_expr)
              | Or of (dep_expr * dep_expr)

type dep_constraint = dep_expr

type dep_main_name = string
type dep_name = { dep_name : dep_main_name
                ; dep_subname : string list
                }

type dependency = dep_name * (dep_constraint option)

type obuild_target =
    { target_name      : string
    ; target_srcdir    : string option
    ; target_cdir      : string option
    ; target_csources  : filename list
    ; target_builddeps : dependency list
    }

type obuild_lib =
    { lib_name      : string
    ; lib_modules   : modname list
    ; lib_builddeps : dependency list
    ; lib_srcdir    : string option
    ; lib_cdir      : string option
    ; lib_csources  : filename list
    ; lib_pack      : bool
    }

type obuild_exe =
    { exe_name      : string
    ; exe_main      : string
    ; exe_srcdir    : string option
    ; exe_builddeps : dependency list
    ; exe_cdir      : string option
    ; exe_csources  : filename list
    }

type obuild_flag =
    { flag_name        : string
    ; flag_description : string
    ; flag_default     : string option
    }

type obuild =
    { obuild_name        : string
    ; obuild_version     : string
    ; obuild_description : string
    ; obuild_license     : string
    ; obuild_authors     : string list
    ; obuild_ver         : int
    ; obuild_homepage    : string
    ; obuild_flags       : obuild_flag list
    ; obuild_libs        : obuild_lib list
    ; obuild_exes        : obuild_exe list
    }

type verbosity = Silent | Report | Verbose | Debug | DebugPlus

type conf_setup = (string * string) list

type general_conf =
    { conf_verbosity : verbosity
    ; conf_withopt   : bool
    ; mutable conf_setup : conf_setup
    }
