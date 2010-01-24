INCLUDES = \
	-I$(top_srcdir)/taglib \
	-I$(top_srcdir)/taglib/toolkit \
	-I$(top_srcdir)/taglib/mpeg/id3v2 \
	$(all_includes)

noinst_LTLIBRARIES = libframes.la

libframes_la_SOURCES = \
	attachedpictureframe.cpp \
	commentsframe.cpp \
	generalencapsulatedobjectframe.cpp \
	popularimeterframe.cpp \
	privateframe.cpp \
	relativevolumeframe.cpp \
	textidentificationframe.cpp \
	uniquefileidentifierframe.cpp \
	unknownframe.cpp \
	unsynchronizedlyricsframe.cpp \
	urllinkframe.cpp

taglib_include_HEADERS = \
	attachedpictureframe.h \
	commentsframe.h \
	generalencapsulatedobjectframe.h \
	popularimeterframe.h \
	privateframe.h \
	relativevolumeframe.h \
	textidentificationframe.h \
	uniquefileidentifierframe.h \
	unknownframe.h \
	unsynchronizedlyricsframe.h \
	urllinkframe.h

taglib_includedir = $(includedir)/taglib
