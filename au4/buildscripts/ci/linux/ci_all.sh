#!/usr/bin/env bash

HERE="$(cd "$(dirname "$0")" && pwd)"

source $HOME/build_tools/environment.sh

cmake -P $HERE/ci_all.cmake
