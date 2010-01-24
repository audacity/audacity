INCLUDES = \
	-I$(top_srcdir)/taglib \
	-I$(top_srcdir)/taglib/toolkit \
	-I$(top_srcdir)/taglib/ape \
	-I$(top_srcdir)/taglib/mpeg/id3v1 \
	-I$(top_srcdir)/taglib/mpeg/id3v2 \
	$(all_includes)

noinst_LTLIBRARIES = libmpc.la

libmpc_la_SOURCES = mpcfile.cpp mpcproperties.cpp

taglib_include_HEADERS = mpcfile.h mpcproperties.h
taglib_includedir = $(includedir)/taglib
