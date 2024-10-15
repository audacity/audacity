#!/bin/sh
set -e

# SoX Resampler Library       Copyright (c) 2007-16 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

test -r throughput.exe && wine=wine

test /$1 = / && list="`seq 0 3`" || list="$*"

for n in $list; do $wine ./throughput 44.1 48 1 0 $n 4; done
