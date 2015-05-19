#  metaflac - Command-line FLAC metadata editor
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

bin_PROGRAMS = metaflac

AM_CFLAGS = @OGG_CFLAGS@
AM_CPPFLAGS = -I$(top_builddir) -I$(srcdir)/include -I$(top_srcdir)/include
EXTRA_DIST = \
	Makefile.lite \
	metaflac.vcproj \
	metaflac.vcxproj \
	metaflac.vcxproj.filters

metaflac_SOURCES = \
	main.c \
	operations.c \
	operations_shorthand_cuesheet.c \
	operations_shorthand_picture.c \
	operations_shorthand_seektable.c \
	operations_shorthand_streaminfo.c \
	operations_shorthand_vorbiscomment.c \
	options.c \
	usage.c \
	utils.c \
	operations.h \
	operations_shorthand.h \
	options.h \
	usage.h \
	utils.h
metaflac_LDFLAGS = $(AM_LDFLAGS)

metaflac_LDADD = \
	$(top_builddir)/src/share/grabbag/libgrabbag.la \
	$(top_builddir)/src/share/replaygain_analysis/libreplaygain_analysis.la \
	$(top_builddir)/src/share/getopt/libgetopt.la \
	$(top_builddir)/src/share/utf8/libutf8.la \
	$(top_builddir)/src/libFLAC/libFLAC.la \
	@LIBICONV@

CLEANFILES = metaflac.exe
