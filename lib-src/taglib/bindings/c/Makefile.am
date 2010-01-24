INCLUDES = \
	-I$(top_srcdir)/taglib \
	-I$(top_srcdir)/taglib/toolkit \
	-I$(top_srcdir)/taglib/asf \
	-I$(top_srcdir)/taglib/mpeg \
	-I$(top_srcdir)/taglib/ogg \
	-I$(top_srcdir)/taglib/ogg/vorbis \
	-I$(top_srcdir)/taglib/ogg/speex \
	-I$(top_srcdir)/taglib/ogg/flac \
	-I$(top_srcdir)/taglib/flac \
	-I$(top_srcdir)/taglib/mpc \
	-I$(top_srcdir)/taglib/mp4 \
	-I$(top_srcdir)/taglib/mpeg/id3v2 \
	-I$(top_srcdir)/taglib/wavpack \
	-I$(top_srcdir)/taglib/trueaudio \
	$(all_includes)

lib_LTLIBRARIES = libtag_c.la

libtag_c_la_SOURCES = tag_c.cpp
taglib_include_HEADERS = tag_c.h
taglib_includedir = $(includedir)/taglib

libtag_c_la_LDFLAGS = $(all_libraries) -no-undefined -version-info 0:0
libtag_c_la_LIBADD = ../../taglib/libtag.la

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = taglib_c.pc
