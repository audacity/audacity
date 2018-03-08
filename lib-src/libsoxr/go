#!/bin/sh
set -e

# SoX Resampler Library       Copyright (c) 2007-16 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

case "$1" in -j*) j="$1"; shift;; esac   # Support -jX for parallel build/test

test x"$1" = x && build=Release || build="$1"

rm -f CMakeCache.txt             # Prevent interference from any in-tree build

mkdir -p "$build"
cd "$build"

cmake -Wno-dev -DCMAKE_BUILD_TYPE="$build" ..
make $j
ctest $j || echo "FAILURE details in $build/Testing/Temporary/LastTest.log"
