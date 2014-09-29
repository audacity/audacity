#!/bin/sh
set -e
# SoX Resampler Library       Copyright (c) 2007-13 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

# Sanity-check of library installed on unix-like system

# This script checks the installation of the entire library (including lsr).
#
# Distros using three separate packages can do the following (in order):
#
# * Install soxr pkg (i.e. basically, just the shared object)
# * ./inst-check-soxr
# * Install soxr-lsr pkg (i.e. basically, just the shared object)
# * ./inst-check-soxr-lsr
# * Install the -dev pkg (i.e. examples, headers, & pkg-config)
# * ./inst-check PATH-OF-INSTALLED-EXAMPLES-DIR (e.g. /usr/share/doc/libsoxr/examples)

# Where are the example source files:
src=$1
test x$src = x && src=/usr/local/share/doc/libsoxr/examples

dir="$(dirname $(readlink -f $0))"
$dir/inst-check-soxr $src
$dir/inst-check-soxr-lsr $src
