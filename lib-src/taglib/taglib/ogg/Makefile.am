SUBDIRS = vorbis flac speex

INCLUDES = -I$(top_srcdir)/taglib -I$(top_srcdir)/taglib/toolkit $(all_includes)

noinst_LTLIBRARIES = libogg.la

libogg_la_SOURCES = \
	oggfile.cpp \
	oggpage.cpp \
	oggpageheader.cpp \
	xiphcomment.cpp

taglib_include_HEADERS = \
	oggfile.h \
	oggpage.h \
	oggpageheader.h \
	xiphcomment.h

taglib_includedir = $(includedir)/taglib

libogg_la_LIBADD = ./vorbis/libvorbis.la ./flac/liboggflac.la ./speex/libspeex.la
