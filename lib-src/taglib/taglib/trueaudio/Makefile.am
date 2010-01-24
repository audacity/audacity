INCLUDES = \
	-I$(top_srcdir)/taglib \
	-I$(top_srcdir)/taglib/toolkit \
	-I$(top_srcdir)/taglib/mpc \
	-I$(top_srcdir)/taglib/mpeg/id3v1 \
	-I$(top_srcdir)/taglib/mpeg/id3v2 \
	$(all_includes)

noinst_LTLIBRARIES = libtrueaudio.la

libtrueaudio_la_SOURCES = trueaudiofile.cpp trueaudioproperties.cpp

taglib_include_HEADERS = trueaudiofile.h trueaudioproperties.h
taglib_includedir = $(includedir)/taglib
