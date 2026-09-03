#!/usr/bin/env bash
#
# Packages the test-VST3 controller (tools/test-vst3-gate/controller, a Qt Widgets app)
# as a self-contained AppImage, so QA can run it without a Qt install.
#
# Reuses the linuxdeploy / linuxdeploy-plugin-qt / appimagetool that make_appimage.sh
# extracts into $HOME/build_tools, so run this after the Package step.
#
# usage: make_controller_appimage.sh <controller binary> <arch: x86_64|aarch64> <icon.png>
# The AppImage is written next to the binary as <name>-<arch>.AppImage.
set -euo pipefail

CONTROLLER="$1"
PACKARCH="$2"
ICON="$3"

[ -f "$CONTROLLER" ] || { echo "error: controller binary not found: $CONTROLLER"; exit 1; }
[ -f "$ICON" ] || { echo "error: icon not found: $ICON"; exit 1; }
if [ "$PACKARCH" == "armv7l" ]; then PACKARCH="armhf"; fi

BUILD_TOOLS=$HOME/build_tools
for tool in linuxdeploy/linuxdeploy linuxdeploy/linuxdeploy-plugin-qt appimagetool/appimagetool; do
  if [ ! -x "$BUILD_TOOLS/$tool" ]; then
    echo "error: $BUILD_TOOLS/$tool missing - run after the Package step, which extracts it (make_appimage.sh)"
    exit 1
  fi
done
export PATH="$BUILD_TOOLS/linuxdeploy:$BUILD_TOOLS/appimagetool:$PATH"
# linuxdeploy-plugin-qt finds Qt through qmake; install-qt-action provides QT_ROOT_DIR.
export QMAKE="${QMAKE:-${QT_ROOT_DIR}/bin/qmake}"

NAME=$(basename "$CONTROLLER")
OUT_DIR=$(cd "$(dirname "$CONTROLLER")" && pwd)
WORK=$(mktemp -d)
APPDIR="$WORK/AppDir"
trap 'rm -rf "$WORK"' EXIT

# linuxdeploy needs a desktop entry and an icon to build an AppDir.
cat > "$WORK/$NAME.desktop" <<DESKTOP
[Desktop Entry]
Type=Application
Name=Audacity test VST3 plugin controller
Exec=$NAME
Icon=$NAME
Categories=Development;
Terminal=false
DESKTOP
cp "$ICON" "$WORK/$NAME.png"

linuxdeploy --appdir "$APPDIR" --executable "$CONTROLLER" \
            --desktop-file "$WORK/$NAME.desktop" --icon-file "$WORK/$NAME.png"
linuxdeploy-plugin-qt --appdir "$APPDIR"
ARCH="$PACKARCH" appimagetool "$APPDIR" "$OUT_DIR/$NAME-$PACKARCH.AppImage"
echo "packaged $OUT_DIR/$NAME-$PACKARCH.AppImage"
