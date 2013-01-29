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
* Descriptive configuration file.
* Easy for users: no rules to mess about, just describe what you want.
* No building dependency apart from OCaml's stdlib: easy to build
* No tool or binary dependencies apart from ocaml compilers
* OCamlfind-like support integrated for faster compilation

How to build a project using obuild
-----------------------------------

obuild supports a few sub commands:

```
    obuild clean
    obuild configure
    obuild init
    obuild build
    obuild install
    obuild doc
    obuild test
    obuild sdist
```

* `clean`:  make sure there's no build by product in the current project
* `configure`: prepare the project by checking dependencies and making sure
             the environment is consistant. If any of the dependencies
             changes, the user will have to re-run the configure step.
             This also allow the user to change flags that impact the project.
* `build`: build every buildable targets defined by the project.
         This will usually build a library or executables.
* `sdist`: create a compressed archive package with the pieces needed to
         distribute it via source code.
* `doc`: build the documentation associated with the sources
* `test`: run unit tests
* `install`: install the necessary files of a library or executable

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
