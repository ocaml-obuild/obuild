obuild
======

A parallel, incremental and declarative build system for ocaml.

Design
------

The goal is to make a very simple build system for users and developers
of OCaml library and programs.

Obuild acts as building black box: user declares only what they want to
build and with which sources, and the build system will consistantly
build it.

The design is based on cabal, and borrow most of the layout and way of
working, adapting parts where necessary to support OCaml fully.

There's no way to build things that obuild have not been designed to do, on
purpose, so that the experience provided is consistant, and all future
improvements to obuild benefits every single programs and libraries using the
obuild build system. Currently unsupported things should be reported to the
issue tracker.

Feature
-------

* Incremental & Parallel build system. only rebuild what's necessary.
* Descriptive configuration file.
* Easy for users: no rules to mess about, just describe what you want.
* No building dependency apart from ocaml's stdlib: easy to build
* No tool or binary dependencies apart from ocaml compilers
* Ocamlfind like support integrated for faster compilation

How to build a project using obuild
-----------------------------------

obuild supports few sub commands:

    obuild clean
    obuild configure
    obuild init
    obuild build
    obuild install
    obuild doc
    obuild test
    obuild sdist

* clean:  make sure there's no build by product in the current project
* configure: prepare the project by checking dependencies and making sure
             the environment is consistant. If any of the dependencies
             changes, the user will have to re-run the configure step.
             This also allow the user to change flags that impact the project.
* build: build every buildable targets defined by the project.
         This will usually build a library or executables.
* sdist: create a compressed archive package with every pieces needed to
         distribute it.
* doc: build the documentation associated with the sources
* test: run every tests
* install: install to a specific destination the necessary files part of
           a library or executable

How to write a project file
---------------------------

A project file is a file that terminate by the .obuild extension.
Only one per project is supported.

The content is completely declarative and is using a simple layout format.
Every normal line need to be in a simple "key: value" format. multi lines
format is supported by indenting (with spaces) the value related to the key.

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

The different target types:

* executable: this create an executable that is going to be installed by default.
* library: create a library that is going to be installed.
* test: create an executable that will not be installed, and will interact with
        obuild according to the test_type field. cabal test will run every built tests
        in a row. for the exit test_type, the exit code is used to signal error (0 = success, any = failure)
* bench: create an executable that will not be install, and will allow to benchmarks,
         some part of the project. This is largely unimplemented and just a placeholder for future development.
* example: create an executable that is not installed, nor compiled by default. you need
           to use configure with --enable-examples. This allow to make sure that examples are compiled with
           the sources to prevent bitrotting. At a later stage that can be used to generate extra documentation.

Declaring an executable
-----------------------

    executable myexec
      main-is: mymain.ml
      src-dir: src
      build-deps: unix

Declaring a library
-------------------

    library mylib
      modules: Module1, Module2
      src-dir: lib
      build-deps: mydep1, mydep2
