
let help_configure =
    [ "Configure --- Prepare to build the package"
    ; ""
    ; "Configure verify that the environment is able to compile the project"
    ; "and this is where the user can tell obuild options to build"
    ; ""
    ; "System settings and user settings are cached, to provide faster"
    ; "access for building task"
    ]

let help_clean =
    [ "Clean --- Cleanup after obuild"
    ; ""
    ; "Remove all by-product of compilation (.cmx, .cmi, .cmo, etc)"
    ; "and remove the dist directory."
    ]

let help_build =
    [ "Build --- Build every buildable bits"
    ; ""
    ; "Build all your different targets (library, executable,"
    ; "tests, benchmarks, example) that are marked as buildable."
    ]

let help_sdist =
    [ "Sdist --- Create a source distribution file (.tar.gz)"
    ; ""
    ; "generate a source distribution file .tar.gz that contains"
    ; "all the necessary bits to distribute to someone else"
    ; "and being able to build and install the package"
    ]

let help_messages =
    [ "clean", help_clean
    ; "configure", help_configure
    ; "build", help_build
    ; "sdist", help_sdist
    ]
