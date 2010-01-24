INCLUDES = \
	-I$(top_srcdir)/taglib \
	-I$(top_srcdir)/taglib/toolkit \
	-I$(top_srcdir)/taglib/mpeg \
	$(all_includes)

noinst_LTLIBRARIES = libid3v1.la

libid3v1_la_SOURCES = id3v1tag.cpp id3v1genres.cpp

taglib_include_HEADERS = id3v1tag.h id3v1genres.h
taglib_includedir = $(includedir)/taglib
