#!/usr/bin/env bash
#
# Lists the testflow test scripts as a GitHub Actions matrix include array:
#
#   {"include":[{"script":"TC1.1_BasicTest.js"},
#               {"script":"TC1.6_....js","status":"DISABLED"}, ...]}
#
# Every top-level .js in share/testflowscripts is a test case (helpers live
# in steps/), so new scripts fan out automatically rather than being silently
# skipped. The output is a single line, suitable for GITHUB_OUTPUT.

# Bash strict-mode: exit on error, unset variable, or failed pipe (pipeline's exit status is that of the last failing command).
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
SCRIPTS_DIR="$HERE/../../../share/testflowscripts"

# Maps disabled test case filenames to the reason for their disabling.
# Disabled test cases still get a matrix job, but it is labeled DISABLED in
# its name and skips all steps. This keeps disabled tests visible in the
# checks UI rather than silently absent.
DISABLED_FILE="$SCRIPTS_DIR/disabled.json"

list_script_filenames() {
    # Bare filenames, one per line; they become the matrix values and thereby
    # the job names. The glob does not descend into steps/.
    (cd "$SCRIPTS_DIR" && ls -1 -- *.js)
}

to_json_array() {
    # One filename per input line -> ["a.js","b.js",...]. Going through jq
    # (command-line JSON processor, rather than hand-assembling the JSON)
    # also escapes any special characters in the filenames.
    jq --raw-input --slurp --compact-output 'split("\n") | map(select(length > 0))'
}

to_matrix_include() {
    # ["a.js",...] -> {"include":[{"script":"a.js"},...]}, tagging entries of
    # the disabled.json map with status: DISABLED, which GitHub appends to
    # the job name, e.g. "testflow-linux (TC1.6_....js, DISABLED)".
    jq --compact-output --slurpfile disabled "$DISABLED_FILE" '
        $disabled[0] as $off
        | { include: map(. as $s | { script: $s } + (if $off | has($s) then { status: "DISABLED" } else {} end)) }'
}

list_script_filenames | to_json_array | to_matrix_include
