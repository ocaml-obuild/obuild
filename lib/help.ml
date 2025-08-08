
let helpConfigure =
    [ "Configure --- Prepare to build the package"
    ; ""
    ; "Configure verify that the environment is able to compile the project"
    ; "and this is where the user can tell obuild options to build"
    ; ""
    ; "System settings and user settings are cached, to provide faster"
    ; "access for building task"
    ]

let helpClean =
    [ "Clean --- Cleanup after obuild"
    ; ""
    ; "Remove all by-product of compilation (.cmx, .cmi, .cmo, etc)"
    ; "and remove the dist directory."
    ]

let helpBuild =
    [ "Build --- Build every buildable bits"
    ; ""
    ; "Build all your different targets (library, executable,"
    ; "tests, benchmarks, example) that are marked as buildable."
    ]

let helpSdist =
    [ "Sdist --- Create a source distribution file (.tar.gz)"
    ; ""
    ; "generate a source distribution file .tar.gz that contains"
    ; "all the necessary bits to distribute to someone else"
    ; "and being able to build and install the package"
    ]

let helpMessages =
    [ "clean", helpClean
    ; "configure", helpConfigure
    ; "build", helpBuild
    ; "sdist", helpSdist
    ]
