#!/usr/bin/env bash
#
# Runs UI checks (screenshot captures) against a local build the way CI does:
# under Xvfb at 1600x1000 @ 96 dpi, software renderer, fresh seeded profile,
# then compares the captures with the references if ImageMagick is installed.
#
#   run_uicheck.sh --all
#   run_uicheck.sh HomePage.js
#
# Requires xvfb (apt install xvfb; imagemagick for the comparison). Set
# BUILD_DIR for a build other than the default. Keep the environment in sync
# with buildscripts/ci/tools/run_testflow_case.sh.

set -euo pipefail

cd "$(dirname "$0")/../.."

command -v xvfb-run > /dev/null || { echo "xvfb-run not found: sudo apt install xvfb"; exit 1; }

BUILD_DIR=${BUILD_DIR:-build/audacity-release}
SCRIPTS_DIR="$PWD/share/testflowscripts"
APP="$BUILD_DIR/src/app/audacity"
test -x "$APP" || { echo "no build at $APP, set BUILD_DIR"; exit 1; }

if [ "${1:-}" = "--all" ]; then
    set --
    for path in "$SCRIPTS_DIR"/uichecks/*.js; do
        name=$(basename "$path")
        grep -q "\"$name\"" "$SCRIPTS_DIR/uichecks/disabled.json" || set -- "$@" "$name"
    done
fi

test $# -gt 0 || { echo "usage: $(basename "$0") [--all | <UI check>...]"; exit 1; }

export MUSE_TESTFLOW_SCRIPTS_PATH="$SCRIPTS_DIR"
export AU_ALLOW_MULTIPLE_PROCESSES=1
export QT_QUICK_BACKEND=software
export LANG=C.UTF-8

failed=
for name in "$@"; do
    # One output directory per check, so the comparison sees only its captures
    export MUSE_TESTFLOW_DATA_PATH="$PWD/$BUILD_DIR/uichecks/${name%.js}"
    rm -rf "$MUSE_TESTFLOW_DATA_PATH"

    # Fresh profile, seeded with the same deterministic UI settings as CI
    SANDBOX=$(mktemp -d)
    export XDG_CONFIG_HOME="$SANDBOX/config" XDG_DATA_HOME="$SANDBOX/data" \
           XDG_CACHE_HOME="$SANDBOX/cache" XDG_STATE_HOME="$SANDBOX/state"
    mkdir -p "$XDG_CONFIG_HOME/Audacity"
    for app in Audacity4 Audacity4Development; do
        cp buildscripts/ci/tools/testflow_profile.ini "$XDG_CONFIG_HOME/Audacity/$app.ini"
    done

    if ! xvfb-run -a -s "-screen 0 1600x1000x24 -dpi 96" \
            "$APP" --test-case "uichecks/$name" --test-case-speed Fast; then
        failed="$failed $name"
        continue
    fi
    echo "captures: $MUSE_TESTFLOW_DATA_PATH/screenshots"

    if command -v compare > /dev/null; then
        CAPTURES_DIR="$MUSE_TESTFLOW_DATA_PATH/screenshots" OUT_DIR="$MUSE_TESTFLOW_DATA_PATH/diff" \
            bash buildscripts/ci/tools/compare_screenshots.sh "$name" || failed="$failed $name"
    else
        echo "compare not found (sudo apt install imagemagick): skipping the comparison"
    fi
    rm -rf "$SANDBOX"
done

test -z "$failed" || { echo "failed:$failed"; exit 1; }
