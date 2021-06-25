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
