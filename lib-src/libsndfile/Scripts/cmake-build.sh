#!/bin/bash -eu

rm -rf CMakeCache.txt CMakeFiles/ _Build

mkdir _Build
cd _Build
cmake ..
cmake --build .
ctest -V
