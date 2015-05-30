#  test_libFLAC - Unit tester for libFLAC
#  Copyright (C) 2000-2009  Josh Coalson
#  Copyright (C) 2011-2014  Xiph.Org Foundation
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License along
#  with this program; if not, write to the Free Software Foundation, Inc.,
#  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

EXTRA_DIST = \
	Makefile.lite \
	test_libFLAC.vcproj \
	test_libFLAC.vcxproj \
	test_libFLAC.vcxproj.filters

AM_CPPFLAGS = -I$(top_builddir) -I$(srcdir)/include -I$(top_srcdir)/include -I$(top_srcdir)/src/libFLAC/include

noinst_PROGRAMS = test_libFLAC

if OS_IS_WINDOWS
win_utf8_lib = $(top_builddir)/src/share/win_utf8_io/libwin_utf8_io.la
endif

test_libFLAC_LDADD = \
	$(top_builddir)/src/share/grabbag/libgrabbag.la \
	$(top_builddir)/src/share/replaygain_analysis/libreplaygain_analysis.la \
	$(top_builddir)/src/test_libs_common/libtest_libs_common.la \
	$(top_builddir)/src/libFLAC/libFLAC-static.la \
	$(win_utf8_lib) @OGG_LIBS@ \
	-lm

test_libFLAC_SOURCES = \
	bitwriter.c \
	decoders.c \
	encoders.c \
	endswap.c \
	format.c \
	main.c \
	metadata.c \
	metadata_manip.c \
	metadata_object.c \
	md5.c \
	bitwriter.h \
	decoders.h \
	encoders.h \
	endswap.h \
	format.h \
	metadata.h \
	md5.h

CLEANFILES = test_libFLAC.exe
