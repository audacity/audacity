SUBDIRS = frames
INCLUDES = \
	-I$(top_srcdir)/taglib \
	-I$(top_srcdir)/taglib/toolkit \
	-I$(top_srcdir)/taglib/mpeg \
	-I$(top_srcdir)/taglib/mpeg/id3v1 \
	 $(all_includes)

noinst_LTLIBRARIES = libid3v2.la

libid3v2_la_SOURCES = \
	id3v2framefactory.cpp id3v2synchdata.cpp id3v2tag.cpp \
	id3v2header.cpp id3v2frame.cpp id3v2footer.cpp \
	id3v2extendedheader.cpp

taglib_include_HEADERS = \
	id3v2extendedheader.h id3v2frame.h id3v2header.h \
	id3v2synchdata.h id3v2footer.h id3v2framefactory.h id3v2tag.h

taglib_includedir = $(includedir)/taglib

if link_zlib
zlib = -lz
endif

libid3v2_la_LIBADD = ./frames/libframes.la $(zlib)
