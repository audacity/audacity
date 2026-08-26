#!/usr/bin/env bash
#
# Runs one testflow script against the AppImage in ./build.artifacts.
#
# Usage: run_testflow_case.sh <script.js> [<subdirectory of testflowscripts>]
#   run_testflow_case.sh TC1.1_BasicTest.js        # a test case
#   run_testflow_case.sh HomePage.js uichecks      # a UI check

set -euo pipefail

SCRIPT="$1"
SUBDIR="${2:-}"
HERE="$(cd "$(dirname "$0")" && pwd)"

APPIMAGE=$(find ./build.artifacts -type f -name '*.AppImage' -print -quit)
test -n "$APPIMAGE" || { echo "AppImage not found in artifact"; exit 1; }

# Extracted rather than mounted, so the runner does not need libfuse2
chmod +x "$APPIMAGE"
"$APPIMAGE" --appimage-extract > /dev/null

SCRIPTS_DIR=$(find "$PWD/squashfs-root" -type d -name testflowscripts -print -quit)
test -n "$SCRIPTS_DIR" || { echo "no testflowscripts in the AppImage, was it a release build?"; exit 1; }
SCRIPT_PATH="$SCRIPTS_DIR${SUBDIR:+/$SUBDIR}/$SCRIPT"
test -f "$SCRIPT_PATH" || { echo "script not in the AppImage: $SCRIPT_PATH"; exit 1; }

# A running instance would swallow the arguments and exit 0
export AU_ALLOW_MULTIPLE_PROCESSES=1
export MUSE_TESTFLOW_DATA_PATH="$PWD/testflow_data"

# Deterministic rendering, for the screenshot comparison of the UI checks:
# a CPU rasterizer (which also disables the graphics-API auto-restart) and a
# fixed language
export QT_QUICK_BACKEND=software
export LANG=C.UTF-8

# Fresh profile, so nothing leaks in from a previous test case, seeded with
# deterministic UI settings (theme, font, no update check)
SANDBOX=$(mktemp -d)
export XDG_CONFIG_HOME="$SANDBOX/config" XDG_DATA_HOME="$SANDBOX/data" \
       XDG_CACHE_HOME="$SANDBOX/cache" XDG_STATE_HOME="$SANDBOX/state"
mkdir -p "$XDG_CONFIG_HOME/Audacity"
# Development builds use the Audacity4Development application name, the others Audacity4
for app in Audacity4 Audacity4Development; do
    cp "$HERE/testflow_profile.ini" "$XDG_CONFIG_HOME/Audacity/$app.ini"
done

rc=0
# Without a window manager the maximize request is ignored, so the window keeps
# its default 1150x800 and the screen only needs to be larger; the DPI
# determines the default timeline zoom
timeout -k 15 300 xvfb-run -a -s "-screen 0 1600x1000x24 -dpi 96" \
    "$PWD/squashfs-root/AppRun" --test-case "$SCRIPT_PATH" --test-case-speed Fast || rc=$?

shopt -s nullglob
for report in "$MUSE_TESTFLOW_DATA_PATH"/reports/*.txt; do
    echo "--- $report"
    cat "$report"
done

test "$rc" -eq 124 && echo "timed out"
exit $rc
