bin_PROGRAMS = tagreader tagreader_c tagwriter framelist strip-id3v1
tagreader_SOURCES = tagreader.cpp
tagreader_c_SOURCES = tagreader_c.c
tagwriter_SOURCES = tagwriter.cpp
framelist_SOURCES = framelist.cpp
strip_id3v1_SOURCES = strip-id3v1.cpp

INCLUDES = \
	-I$(top_srcdir)/taglib \
	-I$(top_srcdir)/taglib/toolkit \
	-I$(top_srcdir)/taglib/ape \
	-I$(top_srcdir)/taglib/mpeg \
	-I$(top_srcdir)/taglib/mpeg/id3v1 \
	-I$(top_srcdir)/taglib/mpeg/id3v2 \
	-I$(top_srcdir)/bindings/c

LDADD = ../taglib/libtag.la
tagreader_c_LDADD = ../bindings/c/libtag_c.la
