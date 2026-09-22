#!/bin/sh
ids=`VAMP_PATH=. vamp-simple-host --list-ids`
testfile=$1
if [ ! -f "$testfile" ]; then
    echo "Usage: valgrinder.sh <testfile.wav>"
    exit 2
fi
for id in $ids; do 
    id=`echo $id | sed 's/^vamp://'`
    echo
    echo "Testing $id on $testfile..."
    echo
    VAMP_PATH=. valgrind vamp-simple-host "$id" "$testfile" -o /dev/null
    echo
done
