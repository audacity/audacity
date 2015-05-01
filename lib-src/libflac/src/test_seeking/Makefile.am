#  test_seeking - Seeking tester for libFLAC
#  Copyright (C) 2004-2009  Josh Coalson
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
	test_seeking.vcproj \
	test_seeking.vcxproj \
	test_seeking.vcxproj.filters

AM_CFLAGS = @OGG_CFLAGS@

AM_CPPFLAGS = -I$(top_builddir) -I$(srcdir)/include -I$(top_srcdir)/include

noinst_PROGRAMS = test_seeking
test_seeking_LDADD = \
	$(top_builddir)/src/libFLAC/libFLAC.la

test_seeking_SOURCES = \
	main.c

CLEANFILES = test_seeking.exe
