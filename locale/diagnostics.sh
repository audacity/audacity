#!/bin/bash

# Report how complete each translation catalog is

# How many messages in total?
total=`grep '^msgid' audacity.pot | wc -l`

#CSV header
echo "Name,Completed,Pct. completed,Bad lines"

declare -i badlines

for filename in `ls *.po`; do
    # If there are errors from msgcmp, then the last line on standard error
    # contains a count of problematic messages; else it won't match the
    # pattern in awk, so assume no errors
    errors=`msgcmp $filename audacity.pot 2>&1 | \
	awk '/msgcmp: found [0-9]* fatal error/ { nn = $3 } END {print 0+nn}'`
    complete=$((total-errors))

    # detect whether this sequence occurs in any .po file.  It will break
    # msgfmt on Windows.
    badlines=`fgrep '#~|' ${filename} | wc -l`

    # produce comma-separated values
    echo "$filename,$complete,$((complete*100/total)),$badlines"
done | sort -n -t , -k3

exit 0
