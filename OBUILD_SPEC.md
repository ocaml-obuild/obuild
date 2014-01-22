# obuild file specification (DRAFT)

Here are the fields accepted in obuild files

The syntax is        field (flags): accepted_values

* field = string
* flags = M (mandatory)
* boolean = true | True | false | False
* accepted_values = comma separated string list | string list | string | boolean

# Section types

* executable
* library
* flag
* test
* bench
* example

# Toplevel fields

These fields are only accepted in the top level, i.e. not in target
sections, etc.

* name (M): string
* version (M): string
* obuild-ver (M): 1
* synopsis:
* description:
* licence | license: string: Licence for the project
* licence-file | license-file: string: Filename of the licence in the project directory
* homepage: string: URL of the homepage of the project
* tools: ?
* authors: string list CSV, Info about the authors, separated by commas
* author: string list: Info about the author
* extra-srcs: ?
* configure-script: ?

# Target fields

## Common fields

* buildable: boolean
* installable: boolean

## OCaml target fields

* builddepends | builddeps | build-deps: string list: ocamlfind library names
* path | srcdir | src-dir: string: sources directory
* preprocessor | pp: string: preprocessor to use
* extra-deps: comma-separated string list: ?
* stdlib: (none | no | standard | core): standard library to link ?

## C target fields

* cdir | c-dir:
* csources | c-sources:
* cflags | c-flags | ccopts | ccopt | c-opts:
* c-libpaths:
* c-libs:
* c-pkgs:

## Library only fields

* sub | subdir | library: NO VALUE (define a new block)
* per: NO VALUE (define a new block)
* modules:
* pack:
* syntax:
* description:

## Executable | Example | Test common fields

* per: NO VALUE (define a new block)
* main | mainis | main-is: string: ml file being the entry point for the executable

## Test only fields

* rundir:
* runopt:
