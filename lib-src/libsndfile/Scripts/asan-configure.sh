#!/bin/bash -eu

CFLAGS="-fsanitize=address -g" CXXFLAGS=${CFLAGS} ./configure

