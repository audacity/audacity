SUBDIRS = aiff wav

INCLUDES = \
	-I$(top_srcdir)/taglib \
	-I$(top_srcdir)/taglib/toolkit \
	$(all_includes)

noinst_LTLIBRARIES = libriff.la

libriff_la_SOURCES = rifffile.cpp

taglib_include_HEADERS = rifffile.h
taglib_includedir = $(includedir)/taglib

libriff_la_LIBADD = ./aiff/libaiff.la ./wav/libwav.la
