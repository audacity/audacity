#!/bin/sh

# Copyright (C) 2006 Erik de Castro Lopo <erikd@mega-nerd.com>
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in
#       the documentation and/or other materials provided with the
#       distribution.
#     * Neither the author nor the names of any contributors may be used
#       to endorse or promote products derived from this software without
#       specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

set -e

function unix_to_dos {
	sed -e "s/\n/\r\n/" $1 > temp_file
	mv -f temp_file $1
}

if [ $# -lt 1 ] || [ $# -gt 2 ]; then
	echo "Usage : Mingw-make-dist.sh <source tarball>."
	exit 1
	fi

TARGZ=$1
if [ ! -f $TARGZ ]; then
	echo "Can't find source tarball."
	fi

TARGZ=$1
if [ ! -f $TARGZ.asc ]; then
	echo "Can't find source tarball signature."
	fi

UNAME=`uname -s`
if [ x$UNAME != "xMINGW32_NT-5.1" ]; then
	echo "Not able to build Win32 binaries on this platform."
	fi

echo "Building MinGW binary/source zip file."

VERSION=`pwd | sed -e "s#.*/##" | sed -e s/libsndfile-//`
BUILD=`echo $VERSION | sed -e "s/\./_/g"`
INSTALL="libsndfile-$BUILD"
ZIPNAME="$INSTALL.zip"

if [ -z "$BUILD" ]; then
	echo "Bad BUILD variable : '$BUILD'"
	exit 1
	fi

if [ ! -d $INSTALL/ ];  then
	mkdir $INSTALL
	fi

if [ ! -f config.status ]; then
	./configure --prefix=`pwd`/$INSTALL/
else
	teststr=`grep "with options" config.status | grep -- --prefix=`
	if [ -z "$teststr" ]; then
		# --disable-static doesn't work.
		./configure --prefix=`pwd`/$INSTALL/
		fi
	fi

if [ ! -f src/.libs/libsndfile-1.dll ]; then
	make all check
	fi

if [ ! -f $INSTALL/bin/libsndfile-1.dll ]; then
	make install
	rm -f $INSTALL/bin/sndfile-regtest.exe
	strip $INSTALL/bin/*.*
	mv $INSTALL/bin/*.* $INSTALL/include/*.* $INSTALL/
	rmdir $INSTALL/bin
	rm -rf $INSTALL/lib
	rmdir $INSTALL/include
	cp src/libsndfile.def $INSTALL/libsndfile-1.def
	cp Win32/README-precompiled-dll.txt Win32/testprog.c $INSTALL/
	unix_to_dos $INSTALL/libsndfile-1.def
	unix_to_dos $INSTALL/sndfile.h
	unix_to_dos $INSTALL/README-precompiled-dll.txt
	unix_to_dos $INSTALL/testprog.c
	fi

if [ ! -f $INSTALL/libsndfile-$VERSION.tar.gz ]; then
	cp $TARGZ $INSTALL/
	if [ -f $TARGZ.asc ]; then
		cp $TARGZ.asc $INSTALL/
		fi
	fi

if [ ! -f $ZIPNAME ]; then
	zip -r $ZIPNAME $INSTALL/
	fi

