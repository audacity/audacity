#!/usr/bin/env bash

HERE="$(cd "$(dirname "$0")" && pwd)"

source /c/build_tools/environment.sh

cmake -P $HERE/ci_all.cmake
