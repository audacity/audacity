#!/usr/bin/env bash

if [[ "$0" == "${BASH_SOURCE}" ]]; then
    echo >&2 "$0: Please source this script instead of running it."
    exit 1
fi

((${BASH_VERSION%%.*} >= 4)) || { echo >&2 "${BASH_SOURCE}: Error: Please upgrade Bash."; return 1; }

function gh_export()
{
    [[ "${GITHUB_ENV-}" ]] || local -r GITHUB_ENV="/dev/null"
    export -- "$@" && printf "%s\n" "$@" >> "${GITHUB_ENV}"
}

repository_root="$(cd "$(dirname "${BASH_SOURCE}")/../.."; echo "${PWD}")"

gh_export GIT_HASH="$(git show -s --format='%H')"
gh_export GIT_HASH_SHORT="$(git show -s --format='%h')"

gh_export AUDACITY_BUILD_TYPE="RelWithDebInfo"
gh_export AUDACITY_INSTALL_PREFIX="${repository_root}/build/install"

gh_export GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD)

if [[ "${OSTYPE}" == msys* ]]; then # Windows

    # On Windows, we pin the MSVC version
    # Compiler version stands to MSVC version here

    compiler_version=$(echo "${AUDACITY_CMAKE_GENERATOR}" | grep -m 1 -Eo "[[:digit:]]+" | head -1)

elif [[ "${OSTYPE}" == darwin* ]]; then # macOS

    compiler_version="$(clang -dumpversion)"

else # Linux & others

    compiler_version="$(cc -dumpversion)"

fi

gh_export COMPILER_VERSION="${compiler_version}"
gh_export CONAN_VERSION="$(conan --version)"
