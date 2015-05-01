#  FLAC - Free Lossless Audio Codec
#  Copyright (C) 2001-2009  Josh Coalson
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

if FLaC__HAS_XMMS
XMMS_DIRS = plugin_common plugin_xmms
endif

if FLaC__WITH_CPPLIBS
CPPLIBS_DIRS = libFLAC++ test_libFLAC++
endif

SUBDIRS = \
	share \
	libFLAC \
	flac \
	metaflac \
	$(XMMS_DIRS) \
	test_grabbag \
	test_libs_common \
	test_libFLAC \
	test_seeking \
	test_streams \
	utils \
	$(CPPLIBS_DIRS)

EXTRA_DIST = \
	Makefile.lite
