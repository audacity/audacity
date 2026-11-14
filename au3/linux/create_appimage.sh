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
    download_appimage_release "audacity/$1" "$1" "$2"
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
    wget -q https://raw.githubusercontent.com/linuxdeploy/linuxdeploy-plugin-gtk/master/linuxdeploy-plugin-gtk.sh
    chmod +x linuxdeploy-plugin-gtk.sh
)
fi

export PATH="${PWD%/}/linuxdeploy:${PATH}"
linuxdeploy --list-plugins

#============================================================================
# Create symlinks
#============================================================================

sed -i 's|env UBUNTU_MENUPROXY=0 ||' "${appdir}/share/applications/audacity.desktop"
ln -sf --no-dereference . "${appdir}/usr"
ln -sf share/applications/audacity.desktop "${appdir}/audacity.desktop"
ln -sf share/icons/hicolor/scalable/apps/audacity.svg "${appdir}/audacity.svg"
ln -sf share/icons/hicolor/scalable/apps/audacity.svg "${appdir}/.DirIcon"

#============================================================================
# Bundle dependencies
#============================================================================

# HACK: Some wxWidget libraries depend on themselves. Add
# them to LD_LIBRARY_PATH so that linuxdeploy can find them.
export LD_LIBRARY_PATH="${appdir}/usr/lib:${appdir}/usr/lib/audacity:${LD_LIBRARY_PATH-}"

# When running on GitHub actions - libararies are sometimes installed into the DEB_HOST_MULTIARCH
# based location
if [ -f "/etc/debian_version" ]; then
   archDir=$(dpkg-architecture -qDEB_HOST_MULTIARCH)
   export LD_LIBRARY_PATH="${appdir}/usr/lib/${archDir}:${appdir}/usr/lib/${archDir}/audacity:${LD_LIBRARY_PATH-}"
fi

# Prevent linuxdeploy setting RUNPATH in binaries that shouldn't have it
mv "${appdir}/bin/findlib" "${appdir}/../findlib"

linuxdeploy --appdir "${appdir}" --plugin gtk # add all shared library dependencies

echo "###########################################"
echo "Cleaning up package"
echo "###########################################"

find "${appdir}/lib/audacity" -maxdepth 1 ! \( -type d \) -exec rm -v {} \;

if [ -f "/etc/debian_version" ]; then
   archDir=$(dpkg-architecture -qDEB_HOST_MULTIARCH)

   if [[ -d "${appdir}/lib/${archDir}/audacity" ]]; then
      find "${appdir}/lib/${archDir}/audacity" -maxdepth 1 ! \( -type d \) -exec rm -v {} \;
   fi
fi

##########################################################################
# Fix permissions
##########################################################################
chmod +x "${appdir}/AppRun.wrapped"

# Put the non-RUNPATH binaries back
mv "${appdir}/../findlib" "${appdir}/bin/findlib"

mv "${appdir}/share/metainfo/audacity.appdata.xml" "${appdir}/share/metainfo/org.audacityteam.Audacity.appdata.xml"
##########################################################################
# BUNDLE REMAINING DEPENDENCIES MANUALLY
##########################################################################

function find_library()
{
  # Print full path to a library or return exit status 1 if not found
  "${appdir}/bin/findlib" "$@"
}

function fallback_library()
{
  # Copy a library into a special fallback directory inside the AppDir.
  # Fallback libraries are not loaded at runtime by default, but they can
  # be loaded if it is found that the application would crash otherwise.
  local library="$1"
  local full_path="$(find_library "$1")"
  local new_path="${appdir}/fallback/${library}"
  mkdir -p "${new_path}" # directory has the same name as the library
  cp -L "${full_path}" "${new_path}/${library}"
  rm -f "${appdir}/lib/${library}"
  # Use the AppRun script to check at runtime whether the user has a copy of
  # this library. If not then add our copy's directory to $LD_LIBRARY_PATH.
}

unwanted_files=(
  lib/libQt5Core.so.5
  lib/libQt5Gui.so.5
  lib/libQt5Widgets.so.5
)

fallback_libraries=(
  libatk-1.0.so.0 # This will possibly prevent browser from opening
  libatk-bridge-2.0.so.0
  libcairo.so.2 # This breaks FFmpeg support
  libcairo-gobject.so.2
  libjack.so.0 # https://github.com/LMMS/lmms/pull/3958
  libportaudio.so # This is required to enable system PortAudio (so Jack is enabled!)
  libgmodule-2.0.so.0 # Otherwise - Manjaro/Arch will crash, because of libgio mismatch
  libgio-2.0.so.0
  libglib-2.0.so.0
  libgobject-2.0.so.0
  libgthread-2.0.so.0
  libpixman-1.so.0 # https://github.com/audacity/audacity/issues/5327
)

for file in "${unwanted_files[@]}"; do
  rm -f "${appdir}/${file}"
done

for fb_lib in "${fallback_libraries[@]}"; do
  fallback_library "${fb_lib}"
done

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
