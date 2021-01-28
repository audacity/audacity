#!/bin/bash

# Report how complete each translation catalog is

# How many messages in total?
total=`grep '^msgid' audacity.pot | wc -l`

for filename in `ls *.po`; do
    # If there are errors from msgcmp, then the last line on standard error
    # contains a count of problematic messages; else it won't match the
    # pattern in awk, so assume no errors
    errors=`msgcmp $filename audacity.pot 2>&1 | \
	awk '/msgcmp: found [0-9]* fatal errors/ { nn = $3 } END {print 0+nn}'`
    complete=$((total-errors))
    # A few spaces after the filename makes the columns mostly aligned
    echo "${filename}   " $'\t' $complete $'\t' "$((complete*100/total)) %"
done | sort -n -k3

echo

# detect whether this sequence occurs in any .po file.  It will break
# msgfmt on Windows.
echo "Files with illegal comment sequence:"
fgrep -l '#~|' *.po

exit 0
