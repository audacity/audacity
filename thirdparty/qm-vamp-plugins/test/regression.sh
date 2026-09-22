#!/bin/bash

set -eu

mydir=$(dirname "$0")

source_url=https://code.soundsoftware.ac.uk/attachments/download/1698/Zweieck-Duell.ogg

testfile="$mydir/tmp/input.ogg"
truncated_testfile="$mydir/tmp/truncated.ogg"

mkdir -p "$mydir/tmp"

if sonic-annotator -v >/dev/null ; then
    :
else
    echo "Failed to find required binary sonic-annotator"
    exit 1
fi

if [ ! -f "$testfile" ]; then
    if wget --version >/dev/null ; then
        wget -O "$testfile" "$source_url"
    else
        curl -o "$testfile" "$source_url"
    fi
fi

dd if="$testfile" of="$truncated_testfile" bs=1024 count=100

ids=$( VAMP_PATH="$mydir/.." sonic-annotator --list )

successes=0
failures=0
skipped=0
total=0
failed_tests=""

for id in $ids ; do

    echo
    echo "Regression testing: $id"

    total=$(($total + 1))

    plugin=$(echo "$id" | cut -d: -f3)
    output=$(echo "$id" | cut -d: -f4)

    nondeterministic=false
    case "$plugin:$output" in
        qm-segmenter:segmentation) nondeterministic=true;;
        *) ;;
    esac
    
    if [ "$nondeterministic" = "true" ]; then
        echo "This plugin is nondeterministic, can't run regression test - skipping"
        skipped=$(($skipped + 1))
        continue
    fi

    bulky=false
    case "$plugin:$output" in
        qm-adaptivespectrogram:output) bulky=true;;
        qm-chromagram:chromagram) bulky=true;;
        qm-constantq:constantq) bulky=true;;
        qm-dwt:wcoeff) bulky=true;;
        qm-mfcc:coefficients) bulky=true;;
    esac

    infile="$testfile"
    if [ "$bulky" = "true" ]; then
        echo "This plugin produces bulky output, using shortened test file"
        infile="$truncated_testfile"
    fi

    mkdir -p "$mydir/regression-obtained/$plugin"
    outfile="$mydir/regression-obtained/$plugin/$output.csv"

    VAMP_PATH="$mydir/.." \
             sonic-annotator \
 	     -d "$id" \
             -w csv \
 	     --csv-omit-filename \
 	     --csv-one-file "$outfile" \
 	     --csv-force \
 	     "$infile"
    
    expected="$mydir/regression-expected/$plugin/$output.csv"

    if cmp "$outfile" "$expected" ; then
        echo "Done, test passed"
        successes=$(($successes + 1))
    else
        echo
        echo "*** FAIL: Result does not match expected output. Diff begins:"
        echo
        diff -u "$outfile" "$expected" | head -20
        failures=$(($failures + 1))
        failed_tests="$failed_tests $plugin:$output"
    fi
done

echo

if [ "$failures" = "0" ]; then
    echo "Done, all tests passed"
    if [ "$skipped" != "0" ]; then
        echo "($skipped test(s) skipped)"
    fi
    exit 0
else
    echo "ERROR: Some tests failed!"
    echo "$successes/$total tests passed"
    echo "$skipped/$total tests were skipped"
    echo "$failures/$total tests failed"
    echo "The following tests failed:$failed_tests"
    exit 1
fi
