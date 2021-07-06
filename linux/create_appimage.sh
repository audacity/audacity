#!/usr/bin/env bash

((${BASH_VERSION%%.*} >= 4)) || { echo >&2 "$0: Error: Please upgrade Bash."; exit 1; }

set -euxo pipefail

readonly appdir="$1" # input path (Audacity install directory)
readonly appimage="$2" # output path to use for created AppImage

#============================================================================
# Helper functions
#============================================================================

function download_github_release()
{
    local -r repo_slug="$1" release_tag="$2" file="$3"
    wget -q --show-progress "https://github.com/${repo_slug}/releases/download/${release_tag}/${file}"
    chmod +x "${file}"
}

function extract_appimage()
{
    # Extract AppImage so we can run it without having to install FUSE
    local -r image="$1" binary_name="$2"
    local -r dir="${image%.AppImage}.AppDir"
    "./${image}" --appimage-extract >/dev/null # dest folder "squashfs-root"
    mv squashfs-root "${dir}" # rename folder to avoid collisions
    ln -s "${dir}/AppRun" "${binary_name}" # symlink for convenience
    rm -f "${image}"
}

function download_appimage_release()
{
    local -r github_repo_slug="$1" binary_name="$2" tag="$3"
    local -r image="${binary_name}-x86_64.AppImage"
    download_github_release "${github_repo_slug}" "${tag}" "${image}"
    extract_appimage "${image}" "${binary_name}"
}

function download_linuxdeploy_component()
{
    local -r component="$1" tag="$2"
    download_appimage_release "linuxdeploy/$1" "$1" "$2"
}

function create_path()
{
    local -r path="$1"
    if [[ -d "${path}" ]]; then
        return 1 # already exists
    fi
    mkdir -p "${path}"
}

#============================================================================
# Fetch AppImage packaging tools
#============================================================================

if create_path "appimagetool"; then
(
    cd "appimagetool"
    download_appimage_release AppImage/AppImageKit appimagetool continuous
)
fi
export PATH="${PWD%/}/appimagetool:${PATH}"
appimagetool --version

if create_path "linuxdeploy"; then
(
    cd "linuxdeploy"
    download_linuxdeploy_component linuxdeploy continuous
)
fi
export PATH="${PWD%/}/linuxdeploy:${PATH}"
linuxdeploy --list-plugins

#============================================================================
# Create symlinks
#============================================================================

ln -sf --no-dereference . "${appdir}/usr"
ln -sf share/applications/audacity.desktop "${appdir}/audacity.desktop"
ln -sf share/icons/hicolor/scalable/apps/audacity.svg "${appdir}/audacity.svg"
ln -sf share/icons/hicolor/scalable/apps/audacity.svg "${appdir}/.DirIcon"

#============================================================================
# Bundle dependencies
#============================================================================

# HACK: Some wxWidget libraries depend on themselves. Add
# them to LD_LIBRARY_PATH so that linuxdeploy can find them.
export LD_LIBRARY_PATH="${appdir}/usr/lib/audacity:${LD_LIBRARY_PATH-}"

linuxdeploy --appdir "${appdir}" # add all shared library dependencies

#============================================================================
# Build AppImage
#============================================================================

appimagetool_args=(
    # none
)

if [[ "${AUDACITY_UPDATE_INFO-}" ]]; then
    # Enable updates. See https://github.com/AppImage/AppImageSpec/blob/master/draft.md#update-information
    appimagetool_args+=( --updateinformation="${AUDACITY_UPDATE_INFO}" )
else
    echo >&2 "$0: Automatic updates disabled"
fi

# Create AppImage
cd "$(dirname "${appimage}")" # otherwise zsync created in wrong directory
appimagetool "${appimagetool_args[@]}" "${appdir}" "${appimage}"
