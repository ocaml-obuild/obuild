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
exit 0
