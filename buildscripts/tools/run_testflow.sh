#!/usr/bin/env bash
#
# Runs testflow test cases against a local build:
#
#   run_testflow.sh --all
#   run_testflow.sh TC1.1_BasicTest.js
#
# Set BUILD_DIR for a build other than the default. Runs against your own
# settings, so a customised toolbar fails the workspace test case.

set -euo pipefail

cd "$(dirname "$0")/../.."

BUILD_DIR=${BUILD_DIR:-build/audacity-release}
SCRIPTS_DIR="$PWD/share/testflowscripts"

case "$(uname)" in
    Darwin) APP="$BUILD_DIR/src/app/audacity.app/Contents/MacOS/audacity" ;;
    *)      APP="$BUILD_DIR/src/app/audacity" ;;
esac
test -x "$APP" || { echo "no build at $APP, set BUILD_DIR"; exit 1; }

if [ "${1:-}" = "--all" ]; then
    set --
    for path in "$SCRIPTS_DIR"/*.js; do
        name=$(basename "$path")
        grep -q "\"$name\"" "$SCRIPTS_DIR/disabled.json" || set -- "$@" "$name"
    done
fi

test $# -gt 0 || { echo "usage: $(basename "$0") [--all | <test case>...]"; exit 1; }

export MUSE_TESTFLOW_SCRIPTS_PATH="$SCRIPTS_DIR"
export MUSE_TESTFLOW_DATA_PATH="$PWD/$BUILD_DIR/testflow_data"
export AU_ALLOW_MULTIPLE_PROCESSES=1
export ASAN_OPTIONS=${ASAN_OPTIONS:-detect_leaks=0:new_delete_type_mismatch=0}

failed=
for name in "$@"; do
    "$APP" --test-case "$name" --test-case-speed Fast || failed="$failed $name"
done

test -z "$failed" || { echo "failed:$failed"; exit 1; }
