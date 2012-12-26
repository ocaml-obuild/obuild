type modname = string

type obuild_lib =
    { lib_modules : modname list
    }

type obuild_exe =
    { exe_name : string
    ; exe_main : string
    ; exe_srcdir : string option
    ; exe_builddeps : string list
    }

type obuild =
    { obuild_name : string
    ; obuild_version : string
    ; obuild_description : string
    ; obuild_license : string
    ; obuild_author : string
    ; obuild_ver : int
    ; obuild_libs : obuild_lib list
    ; obuild_exes : obuild_exe list
    }

type verbosity = Silent | Report | Verbose | Debug

type general_conf =
    { conf_verbosity : verbosity
    }
