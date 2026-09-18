#!/usr/bin/env bash
#
# Runs one testflow test case against the AppImage in ./build.artifacts.

set -euo pipefail

SCRIPT="$1"

APPIMAGE=$(find ./build.artifacts -type f -name '*.AppImage' -print -quit)
test -n "$APPIMAGE" || { echo "AppImage not found in artifact"; exit 1; }

# Extracted rather than mounted, so the runner does not need libfuse2
chmod +x "$APPIMAGE"
"$APPIMAGE" --appimage-extract > /dev/null

SCRIPTS_DIR=$(find "$PWD/squashfs-root" -type d -name testflowscripts -print -quit)
test -n "$SCRIPTS_DIR" || { echo "no testflowscripts in the AppImage, was it a release build?"; exit 1; }

# A running instance would swallow the arguments and exit 0
export AU_ALLOW_MULTIPLE_PROCESSES=1
export MUSE_TESTFLOW_DATA_PATH="$PWD/testflow_data"

# Fresh profile, so nothing leaks in from a previous test case
SANDBOX=$(mktemp -d)
export XDG_CONFIG_HOME="$SANDBOX/config" XDG_DATA_HOME="$SANDBOX/data" \
       XDG_CACHE_HOME="$SANDBOX/cache" XDG_STATE_HOME="$SANDBOX/state"

# Let the test run for five minutes; if it does not exit after TERM, send KILL 15 seconds later
rc=0
timeout -k 15 300 xvfb-run -a -s "-screen 0 1600x1000x24" \
    "$PWD/squashfs-root/AppRun" --test-case "$SCRIPTS_DIR/$SCRIPT" --test-case-speed Fast || rc=$?

shopt -s nullglob
for report in "$MUSE_TESTFLOW_DATA_PATH"/reports/*.txt; do
    echo "--- $report"
    cat "$report"
done

test "$rc" -eq 124 && echo "timed out"
exit $rc
