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
    -D audacity_has_updates_check=yes
    -D SHOW_WHATS_NEW_SECTION=yes
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

if [[ -n "${APPLE_CODESIGN_IDENTITY-}" && "${OSTYPE}" == darwin* ]]; then
    cmake_args+=(
        -D APPLE_CODESIGN_IDENTITY="${APPLE_CODESIGN_IDENTITY}"
        -D audacity_perform_codesign=yes
    )

    if [[ ${GIT_BRANCH} == release* ]]; then
        cmake_args+=(
            -D APPLE_NOTARIZATION_USER_NAME="${APPLE_NOTARIZATION_USER_NAME}"
            -D APPLE_NOTARIZATION_PASSWORD="${APPLE_NOTARIZATION_PASSWORD}"
            -D audacity_perform_notarization=yes
        )
    fi
elif [[ -n "${WINDOWS_CERTIFICATE-}" && "${OSTYPE}" == msys* ]]; then
    # Windows certificate will be used from the environment
    cmake_args+=(
        -D audacity_perform_codesign=yes
    )
fi

if [[ ${GIT_BRANCH} == release* ]]; then
    cmake_args+=(
        -D audacity_package_manual=yes
        -D AUDACITY_BUILD_LEVEL=2
    )
fi

# Configure Audacity
cmake "${cmake_args[@]}"

if [[ "${OSTYPE}" == msys* ]]; then # Windows
    # On Windows, preserve PDB files before clearing the build cache
    
    conanUnixPath=$(cygpath ${CONAN_USER_HOME})
    pdbOutputPath="${conanUnixPath}/pdbs"

    ls -la ${conanUnixPath}

    mkdir -p "${pdbOutputPath}"
    find "${conanUnixPath}/.conan" -name '*.pdb' '!' -name "vc14?.pdb" -type f | xargs -I % cp -v % ${pdbOutputPath}
elif [[ "${OSTYPE}" == darwin* ]]; then # macOS
    # On macOS - find all the .dylib files and generate dSYMs from them 
    # in the same folder.
    # dsymutil requires *.o files, so we need to generate files before clearing
    # the build directories.

    chmod +x scripts/ci/macos/generate_dsym.sh
    scripts/ci/macos/generate_dsym.sh
fi

# Remove build directories and sources to reduce the cache size.
conan remove "*" --src --builds --force
