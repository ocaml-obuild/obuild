let version = Sys.ocaml_version in
ignore (Sys.command "rm -f lib/base/compat.ml");
if version < "4.02.0" then
  Sys.command "cat compat_common.ml compat401.ml > lib/base/compat.ml"
else if version < "4.03.0" then
  Sys.command "cat compat_common.ml compat402.ml > lib/base/compat.ml"
else
  Sys.command "cat compat_common.ml compat403.ml > lib/base/compat.ml"
