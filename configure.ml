let version = Sys.ocaml_version in
ignore(Sys.command "rm -f ext/compat.ml");
if version < "4.02.0" then
  Sys.command "cp -f compat401.ml ext/compat.ml"
else
  Sys.command "cp -f compat402.ml ext/compat.ml"
