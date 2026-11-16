#!/bin/sh
# the JavaScript must exist next to the bytecode and behave like the program
js=dist/build/hello/hello.js
[ -f "$js" ] || { echo "missing $js"; exit 1; }
command -v node >/dev/null 2>&1 || { echo "node not found: skipping run"; exit 0; }
out=$(node "$js")
expected="hello js
answer=42"
[ "$out" = "$expected" ] || { echo "unexpected output: $out"; exit 1; }
