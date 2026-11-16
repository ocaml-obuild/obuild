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

## Web fields (js_of_ocaml / wasm_of_ocaml)

Accepted by every target type. The two "enable" fields only act on
executable, test, example and benchmark targets (on a library they are
ignored, with a warning); on a library, the `-files` fields are passed on to
the web targets that depend on it.

* js-of-ocaml | js_of_ocaml | jsoo | js: `true`, `false` or a flag (`$name`): also translate
  the target with js_of_ocaml: `dist/build/<target>/<output>.js`
* js-flags: words: extra arguments for js_of_ocaml (e.g. `--opt 3`)
* js-files: list: JavaScript runtime files (comma-separated, project-relative; they must exist)
* wasm-of-ocaml | wasm_of_ocaml | wasmoo | wasm: `true`, `false` or a flag: also translate the
  target with wasm_of_ocaml: `dist/build/<target>/<output>.wasm.js` and `<output>.wasm.assets/`
* wasm-flags: words: extra arguments for wasm_of_ocaml
* wasm-files: list: runtime files for wasm_of_ocaml (`.wat`, `.wasm`)

`<output>` is the executable name, with the usual `test-`, `bench-` or `example-` prefix for
the other target kinds.

A target with a web backend is built as bytecode only (whatever the configured
executable modes) and linked with `-no-check-prims`. With executable debugging
configured, `<output>.d.js` / `<output>.d.wasm.js` are built as well, with
`--pretty --source-map-inline --debuginfo`. `jsoo_runtime` files declared in the
META of findlib dependencies are passed to js_of_ocaml automatically.

`obuild configure --web-separate=true` replaces whole-program compilation by
separate compilation (build-runtime, one compile per archive and module, link).

## Test only fields

* rundir:
* runopt:
