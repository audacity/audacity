INCLUDES = \
	-I$(top_srcdir)/taglib \
	-I$(top_srcdir)/taglib/toolkit \
	-I$(top_srcdir)/taglib/ogg \
	-I$(top_srcdir)/taglib/flac \
	$(all_includes)

noinst_LTLIBRARIES = liboggflac.la

liboggflac_la_SOURCES = oggflacfile.cpp

taglib_include_HEADERS = oggflacfile.h
taglib_includedir = $(includedir)/taglib
