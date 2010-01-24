INCLUDES = \
	-I$(top_srcdir)/taglib \
	-I$(top_srcdir)/taglib/toolkit \
	-I$(top_srcdir)/taglib/ogg \
	$(all_includes)

noinst_LTLIBRARIES = libvorbis.la

libvorbis_la_SOURCES = vorbisfile.cpp vorbisproperties.cpp

taglib_include_HEADERS = vorbisfile.h vorbisproperties.h
taglib_includedir = $(includedir)/taglib
