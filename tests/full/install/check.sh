#!/bin/bash
# Regression test for the opam-install path. Two properties:
#  1. `obuild install` must NOT fail when a project declares an executable that was
#     never built (only the library was) — it skips it rather than aborting with a
#     missing build directory. (0.3.0 regressed this and broke every revdep with an
#     unbuilt test/example exe.)
#  2. `obuild install` must still install an executable that WAS built.
OBUILD=$(pwd)/../../../dist/build/obuild/obuild
DEST=$(mktemp -d)
BIN="$DEST/bin"
fail() { echo "check failed: $1"; rm -rf "$DEST"; exit 1; }

# scenario 1 — library only: install succeeds, exe is skipped
"$OBUILD" clean >/dev/null 2>&1
"$OBUILD" configure >/dev/null 2>&1 || fail "configure"
"$OBUILD" build lib-foo >/dev/null 2>&1 || fail "build lib-foo"
"$OBUILD" install --destdir "$DEST" --bindir "$BIN" >/dev/null 2>&1 \
  || fail "install after a library-only build (the 0.3.0 regression)"
[ -f "$DEST/foo/META" ] || fail "library META missing at \$DEST/foo/META"
[ -e "$BIN/footool" ] && fail "an unbuilt executable was installed"

# scenario 2 — full build: the executable is installed too
"$OBUILD" clean >/dev/null 2>&1
rm -rf "${DEST:?}"/*
"$OBUILD" configure >/dev/null 2>&1 || fail "configure (full)"
"$OBUILD" build >/dev/null 2>&1 || fail "full build"
"$OBUILD" install --destdir "$DEST" --bindir "$BIN" >/dev/null 2>&1 \
  || fail "install after a full build"
[ -f "$DEST/foo/META" ] || fail "library META missing after full build"
[ -x "$BIN/footool" ] || fail "built executable missing at \$BIN/footool"

rm -rf "$DEST"
echo "install regression check: OK"
exit 0
