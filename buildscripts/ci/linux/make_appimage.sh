#!/usr/bin/env bash

INSTALL_DIR="$1" # App was installed here
APPIMAGE_NAME="$2" # name for AppImage file (created outside $INSTALL_DIR)
PACKARCH="$3" # architecture (x86_64, aarch64, armv7l)
ENV="$4" # path to environment.sh

source $ENV

if [ -z "$INSTALL_DIR" ]; then echo "error: not set INSTALL_DIR"; exit 1; fi
if [ -z "$APPIMAGE_NAME" ]; then echo "error: not set APPIMAGE_NAME"; exit 1; fi
if [ -z "$PACKARCH" ]; then
  PACKARCH="x86_64"
elif [ "$PACKARCH" == "armv7l" ]; then
  PACKARCH="armhf"
fi

HERE="$(cd "$(dirname "$0")" && pwd)"
ORIGIN_DIR=${PWD}
BUILD_TOOLS=$HOME/build_tools

mkdir -p $BUILD_TOOLS

##########################################################################
# INSTALL APPIMAGETOOL AND LINUXDEPLOY
##########################################################################

function extract_appimage()
{
  # Extract AppImage so we can run it without having to install FUSE
  local -r appimage="$1" binary_name="$2"
  local -r appdir="${appimage%.AppImage}.AppDir"
  "./${appimage}" --appimage-extract >/dev/null # dest folder "squashfs-root"
  mv squashfs-root "${appdir}" # rename folder to avoid collisions
  # wrapper script for convenience
  printf '#!/bin/sh\nexec "%s/AppRun" "$@"\n' "$(readlink -f "${appdir}")" > "${binary_name}"
  chmod +x "${binary_name}"
  rm -f "${appimage}"
}

function install_appimage_tool()
{
  local -r binary_name="$1"
  local -r appimage="${binary_name}-${PACKARCH}.AppImage"
  local tool_path
  tool_path="$(command -v "${binary_name}")" || { echo "error: ${binary_name} not provided via extdeps (require_tool)"; exit 1; }
  cp "${tool_path}" "${appimage}"
  chmod +x "${appimage}"
  extract_appimage "${appimage}" "${binary_name}"
}

if [[ ! -d $BUILD_TOOLS/appimagetool ]]; then
  mkdir $BUILD_TOOLS/appimagetool
  cd $BUILD_TOOLS/appimagetool
  install_appimage_tool appimagetool
  cd $ORIGIN_DIR
fi
export PATH="$BUILD_TOOLS/appimagetool:$PATH"
appimagetool --version

if [[ ! -f $BUILD_TOOLS/linuxdeploy/linuxdeploy ]]; then
  mkdir -p $BUILD_TOOLS/linuxdeploy
  cd $BUILD_TOOLS/linuxdeploy
  install_appimage_tool linuxdeploy
  cd $ORIGIN_DIR
fi
if [[ ! -f $BUILD_TOOLS/linuxdeploy/linuxdeploy-plugin-qt ]]; then
  mkdir -p $BUILD_TOOLS/linuxdeploy
  cd $BUILD_TOOLS/linuxdeploy
  install_appimage_tool linuxdeploy-plugin-qt
  cd $ORIGIN_DIR
fi
export PATH="$BUILD_TOOLS/linuxdeploy:$PATH"
linuxdeploy --list-plugins

if [[ ! -d $BUILD_TOOLS/appimageupdatetool ]]; then
  mkdir $BUILD_TOOLS/appimageupdatetool
  cd $BUILD_TOOLS/appimageupdatetool
  install_appimage_tool appimageupdatetool
  cd $ORIGIN_DIR
fi
if [[ "${UPDATE_INFORMATION}" ]]; then
  export PATH="$BUILD_TOOLS/appimageupdatetool:$PATH"
  appimageupdatetool --version
fi

##########################################################################
# BUNDLE DEPENDENCIES INTO APPDIR
##########################################################################
cd "$(dirname "${INSTALL_DIR}")"
appdir="$(basename "${INSTALL_DIR}")" # directory that will become the AppImage

# Prevent linuxdeploy setting RUNPATH in binaries that shouldn't have it
mv "${appdir}/bin/findlib" "${appdir}/../findlib"

# Remove Qt plugins for MySQL and PostgreSQL to prevent
# linuxdeploy-plugin-qt from failing due to missing dependencies.
# SQLite plugin alone should be enough for our AppImage.
# rm -f ${QT_ROOT_DIR}/plugins/sqldrivers/libqsql{mysql,psql}.so
qt_sql_drivers_path="${QT_ROOT_DIR}/plugins/sqldrivers"
qt_sql_drivers_tmp="/tmp/qtsqldrivers"
mkdir -p "$qt_sql_drivers_tmp"
mv "${qt_sql_drivers_path}/libqsqlmysql.so" "${qt_sql_drivers_tmp}/libqsqlmysql.so"
mv "${qt_sql_drivers_path}/libqsqlpsql.so" "${qt_sql_drivers_tmp}/libqsqlpsql.so"

# Semicolon-separated list of platforms to deploy in addition to `libqxcb.so`.
# Used by linuxdeploy-plugin-qt.
export EXTRA_PLATFORM_PLUGINS="libqoffscreen.so;libqwayland.so"

# Colon-separated list of root directories containing QML files.
# Needed for linuxdeploy-plugin-qt to scan for QML imports.
# Qml files can be in different directories, the qmlimportscanner will go through everything recursively.
export QML_SOURCES_PATHS=./

export LD_LIBRARY_PATH=${appdir}/lib:$LD_LIBRARY_PATH
linuxdeploy --appdir "${appdir}" # adds all shared library dependencies
echo "end linuxdeploy: $?"
linuxdeploy-plugin-qt --appdir "${appdir}" # adds all Qt dependencies
echo "end linuxdeploy-plugin-qt: $?"

# Approximately on June 1, the QtQuick/Controls.2 stopped being deploying
# (at that time the linux deploy was updated).
# This is a hack, for the deployment of QtQuick/Controls.2
if [ ! -f ${appdir}/usr/lib/libQt5QuickControls2.so.5 ]; then
    cp -r ${QT_ROOT_DIR}/qml/QtQuick/Controls.2 ${appdir}/usr/qml/QtQuick/Controls.2
    cp -r ${QT_ROOT_DIR}/qml/QtQuick/Templates.2 ${appdir}/usr/qml/QtQuick/Templates.2
    cp ${QT_ROOT_DIR}/lib/libQt5QuickControls2.so.5 ${appdir}/usr/lib/libQt5QuickControls2.so.5
    cp ${QT_ROOT_DIR}/lib/libQt5QuickTemplates2.so.5 ${appdir}/usr/lib/libQt5QuickTemplates2.so.5
fi

# The system must be used
if [ -f ${appdir}/lib/libglib-2.0.so.0 ]; then
  rm -f ${appdir}/lib/libglib-2.0.so.0
fi

unset QML_SOURCES_PATHS EXTRA_PLATFORM_PLUGINS

# In case this container is reused multiple times, return the moved libraries back
mv "${qt_sql_drivers_tmp}/libqsqlmysql.so" "${qt_sql_drivers_path}/libqsqlmysql.so"
mv "${qt_sql_drivers_tmp}/libqsqlpsql.so" "${qt_sql_drivers_path}/libqsqlpsql.so"

# Put the non-RUNPATH binaries back
mv "${appdir}/../findlib" "${appdir}/bin/findlib"

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
  # Use the AppRun script to check at runtime whether the user has a copy of
  # this library. If not then add our copy's directory to $LD_LIBRARY_PATH.
}

# UNWANTED FILES
# linuxdeploy or linuxdeploy-plugin-qt may have added some files or folders
# that we don't want. List them here using paths relative to AppDir root.
# Report new additions at https://github.com/linuxdeploy/linuxdeploy/issues
# or https://github.com/linuxdeploy/linuxdeploy-plugin-qt/issues for Qt libs.
unwanted_files=(
  # https://github.com/musescore/MuseScore/issues/24068#issuecomment-2297823192
  lib/libwayland-client.so.0
)

# ADDITIONAL QT COMPONENTS
# linuxdeploy-plugin-qt may have missed some Qt files or folders that we need.
# List them here using paths relative to the Qt root directory. Report new
# additions at https://github.com/linuxdeploy/linuxdeploy-plugin-qt/issues
additional_qt_components=(
  plugins/printsupport/libcupsprintersupport.so

  # At an unknown point in time, the libqgtk3 plugin stopped being deployed
  plugins/platformthemes/libqgtk3.so

  # Wayland support (run with QT_QPA_PLATFORM=wayland to use)
  plugins/wayland-decoration-client
  plugins/wayland-graphics-integration-client
  plugins/wayland-shell-integration
)

# ADDITIONAL LIBRARIES
# linuxdeploy may have missed some libraries that we need
# Report new additions at https://github.com/linuxdeploy/linuxdeploy/issues
additional_libraries=(
)

# FALLBACK LIBRARIES
# These get bundled in the AppImage, but are only loaded if the user does not
# already have a version of the library installed on their system. This is
# helpful in cases where it is necessary to use a system library in order for
# a particular feature to work properly, but where the program would crash at
# startup if the library was not found. The fallback library may not provide
# the full functionality of the system version, but it does avoid the crash.
# Report new additions at https://github.com/linuxdeploy/linuxdeploy/issues
fallback_libraries=(
  libjack.so.0 # https://github.com/LMMS/lmms/pull/3958
)

# PREVIOUSLY EXTRACTED APPIMAGES
# These include their own dependencies. We bundle them uncompressed to avoid
# creating a double layer of compression (AppImage inside AppImage).
if [[ "${UPDATE_INFORMATION}" ]]; then
extracted_appimages=(
  appimageupdatetool
)
else
extracted_appimages=(
  # none
)
fi

for file in "${unwanted_files[@]}"; do
  rm -rf "${appdir}/${file}"
done

for file in "${additional_qt_components[@]}"; do
  mkdir -p "${appdir}/$(dirname "${file}")"
  cp -Lr "${QT_ROOT_DIR}/${file}" "${appdir}/${file}"
done

for lib in "${additional_libraries[@]}"; do
  full_path="$(find_library "${lib}")"
  cp -L "${full_path}" "${appdir}/lib/${lib}"
done

for fb_lib in "${fallback_libraries[@]}"; do
  fallback_library "${fb_lib}"
done

for name in "${extracted_appimages[@]}"; do
  symlink="$(which "${name}")"
  apprun="$(dirname "${symlink}")/$(readlink "${symlink}")"
  if [[ ! -L "${symlink}" || ! -f "${apprun}" ]]; then
    echo "$0: Warning: Unable to find AppImage for '${name}'. Will not bundle." >&2
    continue
  fi
  extracted_appdir_path="$(dirname "${apprun}")"
  extracted_appdir_name="$(basename "${extracted_appdir_path}")"
  cp -r "${extracted_appdir_path}" "${appdir}/"
  cat >"${appdir}/bin/${name}" <<EOF
#!/bin/sh
unset APPDIR APPIMAGE # clear outer values before running inner AppImage
HERE="\$(dirname "\$(readlink -f "\$0")")"
exec "\${HERE}/../${extracted_appdir_name}/AppRun" "\$@"
EOF
  chmod +x "${appdir}/bin/${name}"
done

# METHOD OF LAST RESORT
# Special treatment for some dependencies when all other methods fail

# Bundle libnss3 and friends as fallback libraries. Needed on Chromebook.
# See discussion at https://github.com/probonopd/linuxdeployqt/issues/35
libnss3_files=(
  # https://packages.ubuntu.com/xenial/amd64/libnss3/filelist
  libnss3.so
  libnssutil3.so
  libsmime3.so
  libssl3.so
  nss/libfreebl3.chk
  nss/libfreebl3.so
  nss/libfreeblpriv3.chk
  nss/libfreeblpriv3.so
  nss/libnssckbi.so
  nss/libnssdbm3.chk
  nss/libnssdbm3.so
  nss/libsoftokn3.chk
  nss/libsoftokn3.so
)

libnss3_system_path="$(dirname "$(find_library libnss3.so)")"
libnss3_appdir_path="${appdir}/fallback/libnss3.so" # directory named like library

mkdir -p "${libnss3_appdir_path}/nss"

for file in "${libnss3_files[@]}"; do
  cp -L "${libnss3_system_path}/${file}" "${libnss3_appdir_path}/${file}"
  rm -f "${appdir}/lib/$(basename "${file}")" # in case it was already packaged by linuxdeploy
done

##########################################################################
# TURN APPDIR INTO AN APPIMAGE
##########################################################################

appimage="${APPIMAGE_NAME}" # name to use for AppImage file

appimagetool_args=( # array
  --no-appstream # do not check upstream metadata
  )

created_files=(
  "${appimage}"
  )

if [[ "${UPDATE_INFORMATION}" ]]; then
  appimagetool_args+=( # append to array
    --updateinformation "${UPDATE_INFORMATION}"
    )
  created_files+=(
    "${appimage}.zsync" # this file will contain delta update data
    )
else
  cat >&2 <<EOF
$0: Automatic updates disabled.
To enable automatic updates, please set the env. variable UPDATE_INFORMATION
according to <https://github.com/AppImage/AppImageSpec/blob/master/draft.md>.
EOF
fi

# create AppImage
appimagetool "${appimagetool_args[@]}" "${appdir}" "${appimage}"

echo "Making AppImage finished"
