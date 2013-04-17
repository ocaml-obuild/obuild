This is an unexhaustive and probably inaccurate list of items that need to be
looked at or completed to make obuild even better. It is a good source of idea
for anyone wanting to contribute.

Projects file
-------------

* support if/then/else construct in project file.
* add platform and architecture tests in project file: i.e. "if arch(x86) && ..."
* utf8 in project file (maybe useful ?)

Better configuring
------------------

* configure storing / build checking of system state (e.g. digest of libraries, pkg-config, etc)
* cache meta in a friendlier format in dist/ after configure. speed up build.
* arbitrary mechanism to poke at the platform and see what it supports. feeding the file autogeneration phase.
* per project and per system configuration file (à la git)

Perf Improvement
----------------

* use the transitive-edge-reduced dag for checking dependencies.
* remove redundant mtime checks by using a invaliding mtime hashtbl caching mechanism.
* improve change detection with a digest after mtime change.
* improve compilation with .mli by moving the dag pointer of its parents to the compiled interface, not the compiled module.
* ocamldep parallelization & multiples

Completeness
-----------

* add install, and generate META
* generate HTML documentation
* generate cmxs
* generate opam files (.install and .config)
* benchs

Documenting
-----------

* specification for the .obuild file format
* mli files and code documentation

Misc
----

* init: make it better
* add globs for extras source
* add automatic build-deps scanning/adding (see if possible and default to off probably)
* librarify some part of obuild (Config parsing, meta parsing, opam generation, dependencies analysis, building analysis,...)
* replace Digest by a faster (and more modern) digest module from cryptohash
* better portability (windows)
* add a way to refresh a .mli from scratch. for example obuild generate-mli src/ext.ml will (re-)write src/ext.mli
* add a simple way to switch stdlib so that core can be used instead of the compiler stdlib for any target. (project field parsing done already)
* have test (re-)build themselves when doing obuild test, instead of doing 'obuild build; obuild test'.
* improve command line experience (cmdliner ?)
