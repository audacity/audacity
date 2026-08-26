#!/usr/bin/env bash
#
# Lists the testflow scripts of a directory as a GitHub Actions matrix include array:
#
#   {"include":[{"script":"TC1.1_BasicTest.js"},{"script":"TC1.6_...js","status":"DISABLED"}]}
#
# Usage: list_testflow_scripts.sh [<dir relative to the repo root>]
# Default: share/testflowscripts (the test cases). Every top-level .js of the
# directory is a script, helpers live in steps/. Disabled scripts are read from
# the directory's disabled.json.

set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
SCRIPTS_DIR="$HERE/../../../${1:-share/testflowscripts}"
DISABLED_FILE="$SCRIPTS_DIR/disabled.json"

list_script_filenames() {
    (cd "$SCRIPTS_DIR" && ls -1 -- *.js)
}

to_json_array() {
    jq --raw-input --slurp --compact-output 'split("\n") | map(select(length > 0))'
}

to_matrix_include() {
    # GitHub appends status to the job name, e.g. "run (TC1.6_...js, DISABLED)"
    jq --compact-output --slurpfile disabled "$DISABLED_FILE" '
        $disabled[0] as $off
        | { include: map(. as $s | { script: $s } + (if $off | has($s) then { status: "DISABLED" } else {} end)) }'
}

list_script_filenames | to_json_array | to_matrix_include
