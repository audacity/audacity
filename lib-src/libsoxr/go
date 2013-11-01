#!/bin/sh
# SoX Resampler Library       Copyright (c) 2007-12 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

build=$1
test x$build = x && build=Release

rm -f CMakeCache.txt             # Prevent interference from any in-tree build

mkdir -p $build
cd $build

cmake -DCMAKE_BUILD_TYPE=$build -DBUILD_TESTS=ON .. &&
  make &&
    (make test || echo "FAILURE details in $build/Testing/Temporary/LastTest.log")
