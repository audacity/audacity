#!/bin/sh
set -e

aclocal -I m4
autoconf
rm -rf aclocal.m4 autom4te.cache
