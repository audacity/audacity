#!/bin/sh -e

#  FLAC - Free Lossless Audio Codec
#  Copyright (C) 2002-2009  Josh Coalson
#  Copyright (C) 2011-2014  Xiph.Org Foundation
#
#  This file is part the FLAC project.  FLAC is comprised of several
#  components distributed under different licenses.  The codec libraries
#  are distributed under Xiph.Org's BSD-like license (see the file
#  COPYING.Xiph in this distribution).  All other programs, libraries, and
#  plugins are distributed under the GPL (see COPYING.GPL).  The documentation
#  is distributed under the Gnu FDL (see COPYING.FDL).  Each file in the
#  FLAC distribution contains at the top the terms under which it may be
#  distributed.
#
#  Since this particular file is relevant to all components of FLAC,
#  it may be distributed under the Xiph.Org license, which is the least
#  restrictive of those mentioned above.  See the file COPYING.Xiph in this
#  distribution.

. ./common.sh

PATH=../src/test_libFLAC++:$PATH
PATH=../objs/$BUILD/bin:$PATH

run_test_libFLACpp ()
{
	if [ x"$FLAC__TEST_WITH_VALGRIND" = xyes ] ; then
		valgrind --leak-check=yes --show-reachable=yes --num-callers=50 --log-fd=4 test_libFLAC++${EXE} $* 4>>test_libFLAC++.valgrind.log
	else
		test_libFLAC++${EXE} $*
	fi
}

run_test_libFLACpp || die "ERROR during test_libFLAC++"
