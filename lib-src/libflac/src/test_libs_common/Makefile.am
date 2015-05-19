#  test_libs_common - Common code to library unit tests
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

AM_CPPFLAGS = -I$(top_builddir) -I$(srcdir)/include -I$(top_srcdir)/include

noinst_LTLIBRARIES = libtest_libs_common.la

libtest_libs_common_la_SOURCES = \
	file_utils_flac.c \
	metadata_utils.c

EXTRA_DIST = \
	Makefile.lite \
	README \
	test_libs_common_static.vcproj \
	test_libs_common_static.vcxproj \
	test_libs_common_static.vcxproj.filters
