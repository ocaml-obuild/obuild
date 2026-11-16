#!/bin/sh
OBUILD=$(pwd)/../../../dist/build/obuild/obuild
fail() { echo "check failed: $1"; exit 1; }
for f in sum/sum.wasm.js both/both.js both/both.wasm.js; do
  [ -f "dist/build/$f" ] || fail "missing dist/build/$f"
done
ls dist/build/sum/sum.wasm.assets/*.wasm >/dev/null 2>&1 || fail "missing wasm code in sum.wasm.assets"
[ -e dist/build/sum/sum.js ] && fail "sum.js built although js-of-ocaml is off"
command -v node >/dev/null 2>&1 || { echo "node not found: skipping run"; exit 0; }
for f in sum/sum.wasm.js both/both.js both/both.wasm.js; do
  out=$(node "dist/build/$f") || fail "node $f"
  [ "$out" = "sum=10" ] || fail "unexpected output of $f: $out"
done
# the test runs once per backend
"$OBUILD" configure --build-tests=true >/dev/null 2>&1 || fail "configure with tests"
out=$("$OBUILD" test --output 2>&1) || fail "obuild test: $out"
[ "$(echo "$out" | grep -c 'calc ok')" = "2" ] || fail "test did not run under both backends: $out"
# separate compilation gives the same programs
"$OBUILD" configure --web-separate=true >/dev/null 2>&1 || fail "configure separate"
"$OBUILD" build >/dev/null 2>&1 || fail "separate build"
[ -f dist/build/sum/web-wasm/stdlib.wasma ] || fail "no separately compiled stdlib"
for f in sum/sum.wasm.js both/both.js both/both.wasm.js; do
  out=$(node "dist/build/$f") || fail "node $f (separate)"
  [ "$out" = "sum=10" ] || fail "unexpected separate output of $f: $out"
done
exit 0
