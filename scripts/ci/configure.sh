#!/usr/bin/env bash

((${BASH_VERSION%%.*} >= 4)) || { echo >&2 "$0: Error: Please upgrade Bash."; exit 1; }

set -euxo pipefail

conan --version # check it works

cmake_args=(
    -S .
    -B build
    -G "${AUDACITY_CMAKE_GENERATOR}"
    -D audacity_use_pch=no
    -D audacity_has_networking=yes
    -D CMAKE_BUILD_TYPE="${AUDACITY_BUILD_TYPE}"
    -D CMAKE_INSTALL_PREFIX="${AUDACITY_INSTALL_PREFIX}"
)

if [[ "${AUDACITY_CMAKE_GENERATOR}" == "Visual Studio"* ]]; then
    cmake_args+=(
        # skip unneeded configurations
        -D CMAKE_CONFIGURATION_TYPES="${AUDACITY_BUILD_TYPE}"
    )
    case "${AUDACITY_ARCH_LABEL}" in
    32bit)  cmake_args+=( -A Win32 ) ;;
    64bit)  cmake_args+=( -A x64 ) ;;
    *)      echo >&2 "$0: Unrecognised arch label '${AUDACITY_ARCH_LABEL}'" ; exit 1 ;;
    esac
elif [[ "${AUDACITY_CMAKE_GENERATOR}" == Xcode* ]]; then
    cmake_args+=(
        # skip unneeded configurations
        -D CMAKE_CONFIGURATION_TYPES="${AUDACITY_BUILD_TYPE}"
        -T buildsystem=1
    )
fi

# Configure Audacity
cmake "${cmake_args[@]}"
