#!/bin/bash
# findlib exists_if: a transitive dependency whose exists_if files are absent must be
# SKIPPED (as ocamlfind does), not linked. `optabsent`'s exists_if names a file that
# does not exist, so an executable that transitively requires it (via `wrapper`) must
# still link — before the fix, obuild tried to link the absent optabsent archive.
OBUILD=$(pwd)/../../../dist/build/obuild/obuild
FAKELIB=$(pwd)/fakelib
D=$(mktemp -d)
fail() { echo "check failed: $1"; rm -rf "$D"; exit 1; }

cp main.ml "$D/"
cat > "$D/t.obuild" <<'EOF'
obuild-ver: 1
name: t
version: 0.1

executable main
  main-is: main.ml
  build-deps: wrapper
EOF
cd "$D" || fail "cd"
export OCAMLPATH="$FAKELIB"
"$OBUILD" configure >/dev/null 2>&1 || fail "configure"
"$OBUILD" build >/dev/null 2>&1 || fail "build errored on an exists_if-absent transitive dep"
[ -x dist/build/main/main ] || fail "executable not produced"

rm -rf "$D"
echo "exists_if check: OK"
