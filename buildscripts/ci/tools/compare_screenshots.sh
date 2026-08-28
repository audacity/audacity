#!/usr/bin/env bash
#
# Compares the screenshots taken by a UI check with its committed references,
# and fails if any capture is new, changed or missing.
#
# Usage: compare_screenshots.sh <script.js>
#   references: share/testflowscripts/uichecks/references/<script basename>/<capture>.png
#   captures:   $CAPTURES_DIR (default testflow_data/screenshots), written by Audacity.Screenshot
#   on failure: $OUT_DIR/<script basename>/{expected,actual,diff}/ (default ui_checks_diff, the artifact)
#
# SCREENSHOT_FUZZ (default 0%) is passed to ImageMagick's compare -fuzz, an
# escape hatch should exact matching become untenable.

set -euo pipefail

SCRIPT="$1"
NAME="${SCRIPT%.js}"
REFERENCES="share/testflowscripts/uichecks/references/$NAME"
CAPTURES="${CAPTURES_DIR:-testflow_data/screenshots}"
OUT="${OUT_DIR:-ui_checks_diff}/$NAME"
FUZZ="${SCREENSHOT_FUZZ:-0%}"

shopt -s nullglob
captures=("$CAPTURES"/*.png)
references=("$REFERENCES"/*.png)
if [ ${#captures[@]} -eq 0 ] && [ ${#references[@]} -eq 0 ]; then
    echo "no screenshots to compare"
    exit 0
fi

mkdir -p "$OUT/expected" "$OUT/actual" "$OUT/diff"
findings=0
rows=""

for actual in "${captures[@]}"; do
    capture=$(basename "$actual" .png)
    expected="$REFERENCES/$capture.png"
    if [ ! -f "$expected" ]; then
        status="new"; pixels="-"
        cp "$actual" "$OUT/actual/"
        findings=$((findings + 1))
    else
        # -metric AE prints the number of differing pixels; exit 1 when the images differ
        rc=0
        pixels=$(compare -metric AE -fuzz "$FUZZ" "$expected" "$actual" "$OUT/diff/$capture.png" 2>&1) || rc=$?
        if [ "$rc" -eq 0 ]; then
            status="identical"
            rm -f "$OUT/diff/$capture.png"
        elif [ "$rc" -eq 1 ]; then
            status="changed"
            cp "$expected" "$OUT/expected/"
            cp "$actual" "$OUT/actual/"
            findings=$((findings + 1))
        else
            echo "compare failed for $capture: $pixels"
            exit 2
        fi
    fi
    rows+="| $capture | $status | $pixels |"$'\n'
done

for expected in "${references[@]}"; do
    capture=$(basename "$expected" .png)
    if [ ! -f "$CAPTURES/$capture.png" ]; then
        rows+="| $capture | missing | - |"$'\n'
        cp "$expected" "$OUT/expected/"
        findings=$((findings + 1))
    fi
done

table="| Capture | Status | Differing pixels |"$'\n'"|---|---|---|"$'\n'"$rows"
printf '%s' "$table"
{ echo "### UI check: $NAME"; echo; printf '%s' "$table"; echo; } >> "${GITHUB_STEP_SUMMARY:-/dev/null}"

if [ "$findings" -eq 0 ]; then
    rm -rf "$OUT"
    exit 0
fi

echo "::error title=UI check $NAME::$findings screenshot(s) differ from the references"
cat << RECIPE

$findings screenshot(s) are new, changed or missing (see the table above).
Review diff/*.png (on CI: in the ui-checks-$SCRIPT artifact). If the change
is intended, make the captures the new references:

    cp actual/*.png share/testflowscripts/uichecks/references/$NAME/
    git add share/testflowscripts/uichecks/references
    git commit

References must come from CI captures: local renders differ (fonts, renderer).
RECIPE
exit 1
