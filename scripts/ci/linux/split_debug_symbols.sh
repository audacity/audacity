#!/usr/bin/env bash

set -x
objcopy --only-keep-debug --compress-debug-section=zlib "${1}" "${1}.debug"
if [ -f "${1}.debug" ]; then
    objcopy --strip-debug --strip-unneeded "${1}"
    objcopy --add-gnu-debuglink="${1}.debug" "${1}"
fi
