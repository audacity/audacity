#  plugin_common - Routines common to several plugins
#  Copyright (C) 2002-2009  Josh Coalson
#  Copyright (C) 2011-2014  Xiph.Org Foundation
#
#  This library is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public
#  License as published by the Free Software Foundation; either
#  version 2.1 of the License, or (at your option) any later version.
#
#  This library is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with this library; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

AM_CPPFLAGS = -I$(top_builddir) -I$(srcdir)/include -I$(top_srcdir)/include

noinst_LTLIBRARIES = libplugin_common.la

noinst_HEADERS = \
	all.h \
	charset.h \
	defs.h \
	dither.h \
	replaygain.h \
	tags.h

libplugin_common_la_SOURCES = \
	charset.c \
	dither.c \
	replaygain.c \
	tags.c

EXTRA_DIST = \
	Makefile.lite \
	README \
	plugin_common_static.vcproj \
	plugin_common_static.vcxproj \
	plugin_common_static.vcxproj.filters

debug:
	$(MAKE) all CFLAGS="@DEBUG@"

profile:
	$(MAKE) all CFLAGS="@PROFILE@"
