#!/bin/bash
# The bytecode C-stub shared library (dll<clib>.so) must install to stublibs/ (on the
# runtime DLL path), NOT the package dir, so bytecode/toplevel consumers can load it;
# the static libstubs_<clib>.a stays in the package dir for native linking.
OBUILD=$(pwd)/../../../dist/build/obuild/obuild
DEST=$(mktemp -d)
fail() { echo "check failed: $1"; rm -rf "$DEST" driver.ml driver.cm* driver.byte; exit 1; }

"$OBUILD" clean >/dev/null 2>&1
"$OBUILD" configure >/dev/null 2>&1 || fail "configure"
"$OBUILD" build >/dev/null 2>&1 || fail "build"
[ -f dist/build/lib-cstub/dllstubs_cstub.so ] || fail "dllstubs_cstub.so not built"

# direct install: .so -> stublibs/, .a -> package dir, .so NOT duplicated in package dir
"$OBUILD" install --destdir "$DEST" --bindir "$DEST/bin" >/dev/null 2>&1 || fail "install"
[ -f "$DEST/stublibs/dllstubs_cstub.so" ] || fail "stub .so not installed to stublibs/"
[ -f "$DEST/cstub/libstubs_cstub.a" ]     || fail "static stub .a missing from package dir"
[ -e "$DEST/cstub/dllstubs_cstub.so" ]    && fail "stub .so wrongly placed in the package dir"

# opam .install carries a stublibs: entry for the .so
"$OBUILD" install --opam >/dev/null 2>&1 || fail "install --opam"
grep -q 'dllstubs_cstub.so' cstub.install || fail "opam .install missing stublibs entry"

# FUNCTIONAL: a bytecode program using the C stub must LOAD the installed .so at runtime
echo 'let () = Printf.printf "%d\n" (Cstub.cadd 2 3)' > driver.ml
ocamlc -I "$DEST/cstub" -I "$DEST/stublibs" "$DEST/cstub/cstub.cma" driver.ml -o driver.byte 2>/dev/null \
  || fail "bytecode compile against installed lib"
out=$(CAML_LD_LIBRARY_PATH="$DEST/stublibs" ./driver.byte 2>&1) \
  || fail "bytecode run could not load the stub .so: $out"
[ "$out" = "5" ] || fail "wrong output: $out"

rm -rf "$DEST" driver.ml driver.cm* driver.byte cstub.install
echo "cstubs install check: OK"
