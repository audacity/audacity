#!/usr/bin/env bash
#
# Lists the testflow test scripts as a JSON array:
#
#   ["TC1.1_BasicTest.js", "TC1.2_BasicTest.js", ...]
#
# Every top-level .js in share/testflowscripts is a test case (helpers live
# in steps/), so new scripts fan out automatically rather than being silently
# skipped. The output is a single line, suitable for GITHUB_OUTPUT.

# Bash strict-mode: exit on error, unset variable, or failed pipe (pipeline's exit status is that of the last failing command).
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
SCRIPTS_DIR="$HERE/../../../share/testflowscripts"

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

list_script_filenames | to_json_array
