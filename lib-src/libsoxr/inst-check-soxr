#!/bin/sh
set -e
# SoX Resampler Library       Copyright (c) 2007-13 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

# Sanity-check of sub-library installed on unix-like system

arg="$1" # path to installed examples (if dev pkg installed); otherwise omitted
dir="$(dirname $(readlink -f $0))"

# Find the examples:
src="$arg"
test x"$src" = x && src="$dir/examples"
cd $src

# Somewhere to put the binaries:
tmp=`mktemp -d`

build_examples() {
  if [ x"$arg" = x ]; then
    echo "Examples in `pwd`; using local headers:" # for when dev pkg not installed
    libs=-l$1
    cflags=-I$dir/src
  else
    echo "Examples in `pwd`; using pkg-config:"
    libs=$(pkg-config --libs $1)
    cflags=$(pkg-config --cflags $1)
  fi
  for f in ?$2-*.[cC]; do
    cc=cc; echo $f | grep -q C$ && cc=c++
    out=$tmp/`echo $f | sed "s/.[cC]$//"`
    cmd="$cc $cflags -o $out $f $libs -lm"
    echo $cmd; $cmd
  done
}

# Determine library:
if [ `basename $0` = inst-check-soxr ]; then
  build_examples soxr
  gen="dd if=/dev/urandom count=1000"
  $tmp/1-single-block 1 2 .
  $gen 2> /dev/null | $tmp/2-stream                     2>&1 >$tmp/stdout
  $gen 2> /dev/null | $tmp/3-options-input-fn 6 7 2 2 0 2>&1 >$tmp/stdout
  $gen 2> /dev/null | $tmp/4-split-channels   7 6 2 2 3 2>&1 >$tmp/stdout  # Clipping expected here
  $gen 2> /dev/null | $tmp/5-variable-rate              2>&1 >$tmp/stdout
else
  build_examples soxr-lsr a # lsr has 'a' suffix on example number.
  $tmp/1a-lsr 1 2 .
fi

# Tidy up:
rm -rf $tmp
