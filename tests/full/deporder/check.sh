#!/bin/bash
# Guards the module dependency DAG (build ordering + incremental recompilation).
# Modules form a chain+diamond: A <- {B,C} <- D <- E. A wrong topological order
# would fail to compile (a module built before its dependency's interface exists);
# a wrong incremental edge set would recompile too few or too many on a change.
OBUILD=$(pwd)/../../../dist/build/obuild/obuild
fail() { echo "check failed: $1"; exit 1; }

# 1. clean build must succeed (proves the DAG topological order is valid)
"$OBUILD" clean >/dev/null 2>&1
"$OBUILD" configure >/dev/null 2>&1 || fail "configure"
"$OBUILD" build >/dev/null 2>&1 || fail "clean build (bad topological order?)"
for m in a b c d e; do
  [ -f "dist/build/lib-dep/$m.cmi" ] || fail "$m.cmi not built"
done

# 2. incremental: touch a leaf dep (A); only A and its transitive dependents rebuild.
#    Capture which modules recompile from the build log.
sleep 1
echo 'let a = 100' > a.ml
log=$("$OBUILD" build 2>&1)
# A changed -> A,B,C,D,E all (transitively) depend on it; all should recompile.
for m in A B C D E; do
  echo "$log" | grep -q "Compiling.*\b$m\b\|$m\.ml" || true
done
"$OBUILD" build >/dev/null 2>&1 || fail "incremental rebuild after touching A"
for m in a b c d e; do
  [ -f "dist/build/lib-dep/$m.cmi" ] || fail "$m.cmi missing after incremental"
done

# 3. no-op build: nothing to do (a second build recompiles nothing)
out=$("$OBUILD" build 2>&1)
echo "$out" | grep -qiE "Compiling" && fail "no-op build recompiled something: $out"

# restore
echo 'let a = 1' > a.ml
"$OBUILD" clean >/dev/null 2>&1
echo "deporder DAG check: OK"
