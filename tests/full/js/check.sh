#!/bin/sh
# the JavaScript must exist next to the bytecode and behave like the program
OBUILD=$(pwd)/../../../dist/build/obuild/obuild
fail() { echo "check failed: $1"; exit 1; }
js=dist/build/hello/hello.js
[ -f "$js" ] || fail "missing $js"
# the run script configures with executable debugging: a debug variant with
# an inline source map must exist too
[ -f dist/build/hello/hello.d.js ] || fail "missing debug variant hello.d.js"
grep -q "sourceMappingURL" dist/build/hello/hello.d.js || fail "no source map in hello.d.js"
# no native executable, no convenience symlink for a web target
[ -e dist/build/hello/hello ] && fail "a native executable was built"
[ -e hello.byte ] && fail "bytecode symlink left in the project root"
command -v node >/dev/null 2>&1 || { echo "node not found: skipping run"; exit 0; }
expected="hello js!
answer=42"
out=$(node "$js") || fail "node $js"
[ "$out" = "$expected" ] || fail "unexpected output: $out"
out=$(node dist/build/hello/hello.d.js) || fail "node hello.d.js"
[ "$out" = "$expected" ] || fail "unexpected debug output: $out"
# obuild run / obuild test go through node
out=$("$OBUILD" run hello 2>/dev/null | tail -2) || fail "obuild run"
[ "$out" = "$expected" ] || fail "obuild run output: $out"
"$OBUILD" configure --build-tests=true >/dev/null 2>&1 || fail "configure with tests"
out=$("$OBUILD" test --output 2>&1) || fail "obuild test (js test under node): $out"
echo "$out" | grep -q "greet ok" || fail "js test did not run: $out"
# a failed web step must not leave an output that later counts as fresh
"$OBUILD" configure >/dev/null 2>&1 || fail "configure"
"$OBUILD" build >/dev/null 2>&1 || fail "build"
cp lib/greet.ml lib/greet.ml.orig; cp src/extra.js src/extra.js.orig
sed 's/"hello "/"HELLO "/' lib/greet.ml.orig > lib/greet.ml
echo 'function ( {' >> src/extra.js
"$OBUILD" build >/dev/null 2>&1 && fail "a broken runtime file did not fail the build"
sleep 1
cp lib/greet.ml.orig lib/greet.ml; mv src/extra.js.orig src/extra.js; rm -f lib/greet.ml.orig
"$OBUILD" build >/dev/null 2>&1 || fail "build after restoring the sources"
out=$(node "$js") || fail "node $js after a failed build"
[ "$out" = "$expected" ] || fail "stale output after a failed build: $out"
# install: the web executable is skipped, the library's js-files are
# installed and declared in its META (also with a debug configuration, whose
# opt-d directory used to be taken for an executable)
"$OBUILD" configure --executable-debugging=true >/dev/null 2>&1 || fail "configure debug"
"$OBUILD" build >/dev/null 2>&1 || fail "debug build"
DEST=$(mktemp -d)
out=$("$OBUILD" install --destdir "$DEST/lib" --bindir "$DEST/bin" 2>&1) || fail "install: $out"
echo "$out" | grep -q "skipping web executable hello" || fail "web executable not skipped: $out"
[ -f "$DEST/lib/greet/greet_stubs.js" ] || fail "library runtime file not installed"
grep -q 'jsoo_runtime = "greet_stubs.js"' "$DEST/lib/greet/META" || fail "jsoo_runtime missing from META"
# the flag turns the web backend off: a native executable is built instead
"$OBUILD" configure --flag=-web >/dev/null 2>&1 || fail "configure with the web flag off"
"$OBUILD" build >/dev/null 2>&1
[ -e dist/build/hello/hello.js ] && fail "hello.js built although the web flag is off"
# separate compilation gives the same program, and recompiles one unit only
"$OBUILD" configure --web-separate=true --executable-debugging=true >/dev/null 2>&1 || fail "configure separate"
"$OBUILD" build >/dev/null 2>&1 || fail "separate build"
[ -f dist/build/hello/web-js/stdlib.cma.js ] || fail "no separately compiled stdlib"
for f in hello.js hello.d.js; do
  out=$(node dist/build/hello/$f) || fail "node $f (separate)"
  [ "$out" = "$expected" ] || fail "unexpected separate output of $f: $out"
done
cp src/hello.ml src/hello.ml.orig
echo 'let () = print_endline "edited"' >> src/hello.ml
out=$("$OBUILD" build 2>&1)
mv src/hello.ml.orig src/hello.ml
echo "$out" | grep -q "hello.js .*separate: 1 of" || fail "an edit did not recompile exactly one unit: $out"
[ "$(node dist/build/hello/hello.js | tail -1)" = "edited" ] || fail "separate rebuild is stale"
exit 0
