SUBDIRS = id3v1 id3v2
INCLUDES = \
	-I$(top_srcdir)/taglib \
	-I$(top_srcdir)/taglib/toolkit \
        -I$(top_srcdir)/taglib/ape \
	-I$(top_srcdir)/taglib/mpeg/id3v2 -I./id3v2 \
	-I$(top_srcdir)/taglib/mpeg/id3v1 -I./id3v1 \
	$(all_includes)

noinst_LTLIBRARIES = libmpeg.la

libmpeg_la_SOURCES = mpegfile.cpp mpegproperties.cpp mpegheader.cpp xingheader.cpp

taglib_include_HEADERS = mpegfile.h mpegproperties.h mpegheader.h xingheader.h
taglib_includedir = $(includedir)/taglib

libmpeg_la_LIBADD = ./id3v2/libid3v2.la ./id3v1/libid3v1.la
