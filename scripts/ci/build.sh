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

BIN_OUTPUT_DIR=build/bin/${AUDACITY_BUILD_TYPE}
SYMBOLS_OUTPUT_DIR=debug

mkdir ${SYMBOLS_OUTPUT_DIR}

if [[ "${OSTYPE}" == msys* ]]; then # Windows
    # copy PDBs to debug folder...
    find ${BIN_OUTPUT_DIR} -name '*.pdb' | xargs -I % cp % ${SYMBOLS_OUTPUT_DIR}
    # and remove debug symbol files from the file tree before archieving
    find ${BIN_OUTPUT_DIR} -name '*.iobj' -o -name '*.ipdb' -o -name '*.pdb' -o -name '*.ilk' | xargs rm -f
elif [[ "${OSTYPE}" == darwin* ]]; then # macOS
    find ${BIN_OUTPUT_DIR} -name '*.dSYM' | xargs -J % mv % ${SYMBOLS_OUTPUT_DIR}
else # Linux & others
    chmod +x scripts/ci/linux/split_debug_symbols.sh
    find ${BIN_OUTPUT_DIR} -type f -executable -o -name '*.so' | xargs -n 1 scripts/ci/linux/split_debug_symbols.sh
    find ${BIN_OUTPUT_DIR} -name '*.debug' | xargs -I % mv % ${SYMBOLS_OUTPUT_DIR}
fi
