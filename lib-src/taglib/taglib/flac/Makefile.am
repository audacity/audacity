INCLUDES = \
	-I$(top_srcdir)/taglib \
	-I$(top_srcdir)/taglib/toolkit \
	-I$(top_srcdir)/taglib/ogg \
	-I$(top_srcdir)/taglib/mpeg/id3v2 \
	-I$(top_srcdir)/taglib/mpeg/id3v1 \
	$(all_includes)

noinst_LTLIBRARIES = libflac.la

libflac_la_SOURCES = flacfile.cpp flacproperties.cpp

taglib_include_HEADERS = flacfile.h flacproperties.h
taglib_includedir = $(includedir)/taglib
