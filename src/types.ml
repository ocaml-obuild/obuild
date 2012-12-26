type modname = string
type dependency = string

type flag_tweak = SetFlag of string | ClearFlag of string

type obuild_lib =
    { lib_modules : modname list
    ; lib_builddeps : dependency list
    ; lib_srcdir : string option
    }

type obuild_exe =
    { exe_name : string
    ; exe_main : string
    ; exe_srcdir : string option
    ; exe_builddeps : dependency list
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
    ; obuild_author      : string
    ; obuild_ver         : int
    ; obuild_homepage    : string
    ; obuild_flags       : obuild_flag list
    ; obuild_libs        : obuild_lib list
    ; obuild_exes        : obuild_exe list
    }

type verbosity = Silent | Report | Verbose | Debug

type general_conf =
    { conf_verbosity : verbosity
    ; conf_withopt   : bool
    }
