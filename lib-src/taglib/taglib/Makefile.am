SUBDIRS = toolkit mpeg ogg flac ape mpc wavpack trueaudio riff asf mp4

INCLUDES = \
	-I$(top_srcdir)/taglib \
	-I$(top_srcdir)/taglib/toolkit \
	-I$(top_srcdir)/taglib/mpeg \
	-I$(top_srcdir)/taglib/ogg \
	-I$(top_srcdir)/taglib/ogg/flac \
	-I$(top_srcdir)/taglib/flac \
	-I$(top_srcdir)/taglib/mpc \
	-I$(top_srcdir)/taglib/asf \
	-I$(top_srcdir)/taglib/mp4 \
	-I$(top_srcdir)/taglib/ogg/vorbis \
	-I$(top_srcdir)/taglib/ogg/speex \
	-I$(top_srcdir)/taglib/wavpack \
	-I$(top_srcdir)/taglib/trueaudio \
	-I$(top_srcdir)/taglib/riff \
	-I$(top_srcdir)/taglib/riff/aiff \
	-I$(top_srcdir)/taglib/riff/wav \
	-I$(top_srcdir)/taglib/mpeg/id3v2 \
	$(all_includes)

lib_LTLIBRARIES = libtag.la

libtag_la_SOURCES = tag.cpp tagunion.cpp fileref.cpp audioproperties.cpp
taglib_include_HEADERS = tag.h fileref.h audioproperties.h taglib_export.h
taglib_includedir = $(includedir)/taglib

libtag_la_LDFLAGS = $(all_libraries) -no-undefined -version-info 6:0:5
libtag_la_LIBADD = ./mpeg/libmpeg.la ./ogg/libogg.la ./flac/libflac.la ./mpc/libmpc.la \
	./ape/libape.la ./toolkit/libtoolkit.la ./wavpack/libwavpack.la \
	./trueaudio/libtrueaudio.la ./riff/libriff.la \
	./mp4/libmp4.la ./asf/libasf.la
