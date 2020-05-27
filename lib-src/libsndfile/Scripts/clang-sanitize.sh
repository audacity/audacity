#!/bin/bash

# This is known to work with clang-3.4 from Debian testing/unstable.
# 2013/07/14

export CC=clang
export CXX=clang++
export CFLAGS="-O3 -fsanitize=address,integer,undefined"
export CXXFLAGS="-O3 -fsanitize=address,integer,undefined"

./configure --enable-gcc-werror

make clean all check
