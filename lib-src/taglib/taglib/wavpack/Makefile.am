INCLUDES = \
	-I$(top_srcdir)/taglib \
	-I$(top_srcdir)/taglib/toolkit \
	-I$(top_srcdir)/taglib/ape \
	-I$(top_srcdir)/taglib/mpc \
	-I$(top_srcdir)/taglib/mpeg/id3v1 \
	-I$(top_srcdir)/taglib/mpeg/id3v2 \
	$(all_includes)

noinst_LTLIBRARIES = libwavpack.la

libwavpack_la_SOURCES = wavpackfile.cpp wavpackproperties.cpp

taglib_include_HEADERS = wavpackfile.h wavpackproperties.h
taglib_includedir = $(includedir)/taglib
