#!/bin/bash

set -eu

MYDIR=$(dirname "$0")

TEST_PLUGIN_DIR="$MYDIR/../../vamp-test-plugin"
HOST_DIR="$MYDIR/../host"
HOST="$HOST_DIR/vamp-simple-host"
TEST_FILE="$MYDIR/testsignal.wav"

mkdir -p "$MYDIR/obtained"
mkdir -p "$MYDIR/failures"

echo "Rebuilding SDK and simple host..." 1>&2
( cd "$MYDIR/.." && ./configure && make clean && make )

if [ ! -d "$TEST_PLUGIN_DIR" ]; then
    echo "Can't find test plugin dir at $TEST_PLUGIN_DIR" 1>&2
    exit 1
fi

if [ ! -x "$HOST" ]; then
    echo "Can't find host at $HOST" 1>&2
    exit 1
fi

echo "Rebuilding test plugin..." 1>&2
( cd "$TEST_PLUGIN_DIR" && make -f Makefile.linux clean && make -f Makefile.linux )

export VAMP_PATH="$TEST_PLUGIN_DIR"

# check that the two expected test plugin ids are present:

ids=$("$HOST" --list-ids)

expected="vamp:vamp-test-plugin:vamp-test-plugin
vamp:vamp-test-plugin:vamp-test-plugin-freq"

if [ "$ids" != "$expected" ]; then
    echo "Unexpected id list: $ids" 1>&2
    echo "Expected: $expected" 1>&2
    exit 1
fi

some_failed=nope
echo

for test in $("$HOST" --list-outputs | sed 's/^vamp://') ; do

    filename="$(echo "$test.txt" | sed 's/^[^:]*://' | sed 's/:/_/g')"
    expected="$MYDIR/expected/$filename"
    obtained="$MYDIR/obtained/$filename"
    failure="$MYDIR/failures/$filename"

    rm -f "$failure"
    echo "=== $test" > "$obtained"
    "$HOST" "$test" "$TEST_FILE" >> "$obtained" 2>/dev/null
    
    if cmp -s "$expected" "$obtained" ; then
	echo "$test: ok"
    else
	sdiff "$expected" "$obtained" > "$failure" || true # avoid exit-on-failure
	echo "*** $test: FAILED, see $failure for diff"
	some_failed=yup
    fi
    
done

if [ "$some_failed" != "nope" ]; then
    echo; echo "*** Some tests failed!"; echo
fi

