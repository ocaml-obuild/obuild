#!/bin/bash
# An executable that depends on a library living in its own source directory
# must use the library's module, not compile a second copy of it: the copy
# shared the library's build step, was checked against an archive that did
# not exist yet, got recompiled by the next build, and was linked twice.
OBUILD=$(pwd)/../../../dist/build/obuild/obuild
fail() { echo "check failed: $1"; exit 1; }
[ "$(./dist/build/prog/prog)" = "42" ] || fail "wrong program output"
ls dist/build/prog/util.* >/dev/null 2>&1 && fail "the executable compiled its own copy of the library module"
out=$("$OBUILD" build 2>&1)
echo "$out" | grep -qiE "Compiling|Intfing|Linking" && fail "second build is not a no-op: $out"
exit 0
