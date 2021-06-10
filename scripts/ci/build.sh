#!/usr/bin/env bash

((${BASH_VERSION%%.*} >= 4)) || { echo >&2 "$0: Error: Please upgrade Bash."; exit 1; }

set -euxo pipefail

if [[ "${OSTYPE}" == msys* ]]; then # Windows

    cpus="${NUMBER_OF_PROCESSORS}"

elif [[ "${OSTYPE}" == darwin* ]]; then # macOS

    cpus="$(sysctl -n hw.ncpu)"

else # Linux & others

    cpus="$(nproc)"

fi

# Build Audacity
cmake --build build -j "${cpus}" --config "${AUDACITY_BUILD_TYPE}"
