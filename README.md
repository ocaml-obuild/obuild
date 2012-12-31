obuild
======

A parallel, incremental and declarative build system for ocaml.

Design
------

The design is completely based on the building part of cabal-install.
The goal is to make a very simple build system for users and developers
of OCaml library and programs.

Obuild acts as building black box: user declares only what they want to
build and with which sources, and the build system will consistantly
build it.

There's no way to build things that obuild have no been designed to do, on
purpose, so that the experience provided is consistant, and all future
improvements to obuild benefits every single programs and libraries using the
obuild build system.

How to build a project using obuild
-----------------------------------

obuild supports very few sub commands:

    obuild clean
    obuild configure
    obuild build
    obuild sdist

* clean:  make sure there's no build by product in the current project
* configure: prepare the project by checking dependencies and making sure
             the environment is consistant. If any of the dependencies
             changes, the user will have to re-run the configure step.
             This also allow the user to change flags that will have an impact
* build: build every buildable targets defined by the project.
         This will usually build a library or executables.
* sdist: create a compressed archive package with every pieces needed to
         distribute it.

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

Declaring an executable

    executable myexec
      main-is: mymain.ml
      src-dir: src
      build-deps: unix
