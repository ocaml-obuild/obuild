obuild
======

A parallel, incremental and declarative build system for OCaml.

Design
------

The goal is to make a very simple build system for users and developers of
OCaml library and programs.

`obuild` acts as building black box: user declare only what they want to build
and with which sources, and it will be consistently built.  The design is based
on Haskell's Cabal, and borrow most of the layout and way of working,
adapting parts where necessary to support OCaml fully.

There's no way to build things that `obuild` has not been designed to do on
purpose, so that the experience provided is consistent, and all future
improvements to `obuild` will automatically benefit program and libraries using
older versions.  Currently unsupported features should be requested on the
Github issue tracker.

Feature
-------

* Incremental & parallel build system. only rebuilding what's necessary.
* Content-digest based staleness: rebuilds are driven by what actually
  changed, not file timestamps, with early cutoff (an implementation-only
  change does not recompile dependents whose interfaces are unchanged).
* Descriptive configuration file.
* Easy for users: no rules to mess about, just describe what you want.
* No building dependency apart from OCaml's stdlib: easy to build
* No tool or binary dependencies apart from ocaml compilers
* OCamlfind-like support integrated for faster compilation
* Wrapped libraries (`pack: true`): a library's modules namespaced under a
  single top-level module, so different libraries can reuse module names
* Vendored sub-projects (`vendor-dirs:`): build against obuild projects
  checked into your tree (e.g. git submodules) without installing them
* `obuild run`: build and run an executable in one step
* Generates `.merlin` automatically for IDE support

How to build a project using obuild
-----------------------------------

obuild supports a clean, user-friendly command-line interface with helpful error messages and automatic help generation.

### Quick Start

```bash
# Get help
obuild -h                    # Show all available commands
obuild build -h              # Show build-specific options

# Check version
obuild --version

# Typical workflow
obuild configure --build-tests true
obuild build
obuild test
```

### Available Commands

```
obuild configure    Prepare to build the package
obuild build        Make this package ready for installation
obuild run          Build an executable (if needed) and run it
obuild clean        Clean up after a build
obuild install      Install this package
obuild test         Run the tests
obuild doc          Generate documentation
obuild sdist        Generate a source distribution file (.tar.gz)
obuild init         Initialize a new project
obuild get          Get project metadata field
obuild generate     Generate configuration files (merlin, opam, completions)
```

### Command Details

**configure** - Prepare the project by checking dependencies and setting build options

```bash
obuild configure [OPTIONS]

Options:
  --build-tests true|false        Build tests
  --build-examples true|false     Build examples
  --library-bytecode true|false   Compile libraries as bytecode
  --library-native true|false     Compile libraries as native
  -g                              Enable debugging symbols
  --annot                         Generate .annot files
```

**build** - Build every buildable target defined by the project

```bash
obuild build [OPTIONS] [TARGETS...]

Options:
  -j, --jobs N               Maximum number of parallel jobs (default: auto-detected)
  --dot                      Dump dependency graphs as .dot files

Examples:
  obuild build                # Build everything
  obuild build -j 4          # Build with 4 parallel jobs
  obuild build mylib myexe   # Build specific targets only
```

**run** - Build an executable target if it is stale, then run it

```bash
obuild run [EXE] [ARGS...]

# with a single executable in the project the name can be omitted;
# everything after the executable name is passed to the program verbatim
obuild run
obuild run myexec --port 8080
```

The program inherits the terminal and its exit code is propagated.

**clean** - Remove all build artifacts

**test** - Run all test targets

```bash
obuild test [OPTIONS]

Options:
  --output                   Show test output (default: only show failures)
```

**install** - Install libraries and executables

```bash
obuild install [OPTIONS]

Options:
  --destdir DIR              Override installation directory
  --opam                     Generate .install file for OPAM
```

**get** - Retrieve project metadata

```bash
obuild get FIELD

Fields: name, version, license

Examples:
  obuild get name            # Get project name
  obuild get version         # Get project version
```

### Global Options

These options work with any command:

```bash
-v, --verbose              Verbose output
-q, --quiet                Quiet mode (errors only)
--color                    Enable colored output
--strict                   Enable strict mode
```

### Configuration Files

Obuild supports configuration files for setting default values. Config files use a simple `key = value` format.

**Config file locations** (in order of precedence):

1. `./.obuildrc` - Project-specific settings
2. `~/.obuildrc` - User-wide settings

**Example config file:**

```bash
# ~/.obuildrc - User configuration for obuild

# Set default number of parallel jobs
jobs = 8

# Enable colored output by default
color = true

# Verbose mode
verbose = false
```

**Supported options:**

- `jobs` - Default number of parallel build jobs (integer)
- `color` - Enable colored output (true/false)
- `verbose` - Verbose output mode (true/false)
- `quiet` - Quiet mode (true/false)
- `strict` - Strict mode (true/false)

Command-line arguments always override config file values.

### Shell Completion

Obuild can generate shell completion scripts for bash, zsh, and fish:

```bash
# Generate and install bash completion
obuild completion bash > ~/.bash_completion.d/obuild
source ~/.bash_completion.d/obuild

# Generate zsh completion
obuild completion zsh > ~/.zsh/completions/_obuild

# Generate fish completion
obuild completion fish > ~/.config/fish/completions/obuild.fish
```

How to write a project file
---------------------------

A project file is a file terminated by the `.obuild` extension.
Only one per project is supported.

The content is declarative using a simple layout format.
Every normal line needs to be in a "key: value" format. Multiple lines
are supported by indenting (with spaces) the value related to the key.

```
    name: myproject
    version: 0.0.1
    description:
      This is my new cool project
      .
      This is a long description describing properly what the project does.
    licence: MyLicense
    authors: John Doe <john@doe.com>
    obuild-ver: 1
    homepage: http://my.server.com/myproject
```

The different target types:

* executable: this creates an executable that is going to be installed by default.
* library: create a library that is going to be installed.
* test: create an executable that will not be installed, and will interact with
        obuild according to the test_type field. cabal test will run every built tests
        in a row. for the exit test_type, the exit code is used to signal error (0 = success, anything else = failure)
* bench: create an executable that will not be installed, and will allow to benchmarks,
         some part of the project. This is largely unimplemented and just a placeholder for future development.
* example: create an executable that is not installed, nor compiled by default. you need
           to use configure with --enable-examples. This allow to make sure that examples are compiled with
           the sources to prevent bitrotting. At a later stage that can be used to generate extra documentation.

Declaring an executable
-----------------------

```
    executable myexec
      main-is: mymain.ml
      src-dir: src
      build-deps: unix
```

Declaring a library
-------------------

```
    library mylib
      modules: Module1, Module2
      src-dir: lib
      build-deps: mydep1, mydep2
```

Wrapped libraries (pack)
------------------------

Adding `pack: true` to a library namespaces all of its modules under a single
top-level module named after the library:

```
    library mylib
      modules: Util, Core
      src-dir: lib
      pack: true
```

Consumers see `Mylib.Util` and `Mylib.Core`; the bare names are hidden, so two
libraries may both define a module `Util` without clashing.  Inside the
library, modules refer to each other by their plain names.  On OCaml >= 4.02
this is implemented with module aliases (dune-style `Mylib__Util` units, cheap
incremental builds); on older compilers it falls back to `-pack`.  The project
syntax is the same either way.

Without `pack: true`, a module name may only be defined once per project:
obuild reports a clear error when two targets provide the same module name
from different source files.

Vendored sub-projects
---------------------

```
    name: myproject
    ...
    vendor-dirs: vendor
```

Each entry of `vendor-dirs` either is a vendored obuild project directory or
contains project directories (`vendor/*/x.obuild` — the layout produced by
git submodules).  The libraries of vendored projects are built as part of
your project (their `build-deps` resolve against other vendored or project
libraries first, then ocamlfind) and are not installed: they link into your
artifacts.  Vendored executables and tests are ignored.  Vendored projects
must themselves be obuild projects.
