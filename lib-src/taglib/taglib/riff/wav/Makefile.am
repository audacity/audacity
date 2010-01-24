INCLUDES = \
	-I$(top_srcdir)/taglib \
	-I$(top_srcdir)/taglib/toolkit \
	-I$(top_srcdir)/taglib/riff \
	-I$(top_srcdir)/taglib/mpeg/id3v2 \
	$(all_includes)

noinst_LTLIBRARIES = libwav.la

libwav_la_SOURCES = wavfile.cpp wavproperties.cpp

taglib_include_HEADERS = wavfile.h wavproperties.h
taglib_includedir = $(includedir)/taglib
