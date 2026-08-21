#!/bin/bash
# A module shared by two targets (CompileModule is keyed by Hier only) can be stale in
# one target's build dir while fresh in another's. Deciding from only the head
# (cstate,target) pair either silently skips the stale copy (-> "Unbound module" at a
# dependent, or a link failure) or, with an interface present, hands the scheduler an
# empty task group whose completion starts at 0 and never fires (the build hangs).
# Both directions must rebuild correctly, and an up-to-date build must recompile nothing.
OBUILD=$(pwd)/../../../dist/build/obuild/obuild
fail() { echo "check failed: $1"; exit 1; }

# direction 1: the executable's copy of the shared module goes stale
"$OBUILD" clean >/dev/null 2>&1
"$OBUILD" configure >/dev/null 2>&1 || fail "configure"
"$OBUILD" build >/dev/null 2>&1 || fail "clean build"
rm -f dist/build/prog/util.cm* dist/build/prog/util.o
timeout 40 "$OBUILD" build >/dev/null 2>&1 || fail "rebuild after exe's shared module went stale (hang or error)"
[ -x dist/build/prog/prog ] || fail "executable missing (shared module not recompiled for it)"

# direction 2: the library's copy goes stale
"$OBUILD" clean >/dev/null 2>&1
"$OBUILD" configure >/dev/null 2>&1 || fail "configure (2)"
"$OBUILD" build >/dev/null 2>&1 || fail "clean build (2)"
rm -f dist/build/lib-sharedmod/util.cm* dist/build/lib-sharedmod/util.o
timeout 40 "$OBUILD" build >/dev/null 2>&1 || fail "rebuild after lib's shared module went stale"
[ -f dist/build/lib-sharedmod/util.cmi ] || fail "library's shared module not rebuilt"

# up-to-date: a second build recompiles nothing
out=$("$OBUILD" build 2>&1)
echo "$out" | grep -qiE "Compiling|Intfing" && fail "no-op build recompiled: $out"

"$OBUILD" clean >/dev/null 2>&1
echo "shared-module staleness check: OK"
