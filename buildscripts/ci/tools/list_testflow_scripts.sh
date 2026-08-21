#!/usr/bin/env bash
#
# Lists the testflow test cases as a GitHub Actions matrix include array:
#
#   {"include":[{"script":"TC1.1_BasicTest.js"},{"script":"TC1.6_...js","status":"DISABLED"}]}
#
# Every top-level .js is a test case, helpers live in steps/.

set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
SCRIPTS_DIR="$HERE/../../../share/testflowscripts"
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
