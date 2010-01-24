## Process this file with automake to produce Makefile.in

bin_PROGRAMS = sndfile-info sndfile-play sndfile-convert sndfile-jackplay sndfile-cmp \
				sndfile-metadata-set sndfile-metadata-get

OS_SPECIFIC_CFLAGS = @OS_SPECIFIC_CFLAGS@
OS_SPECIFIC_LINKS = @OS_SPECIFIC_LINKS@

INCLUDES = -I$(top_srcdir)/src $(OS_SPECIFIC_CFLAGS)

# This is the BeOS version of sndfile-play. It needs to be compiled with the C++
# compiler.
EXTRA_DIST = sndfile-play-beos.cpp test-sndfile-metadata-set.py

sndfile_info_SOURCES = sndfile-info.c
sndfile_info_LDADD = $(top_builddir)/src/libsndfile.la

sndfile_play_SOURCES = sndfile-play.c
sndfile_play_LDADD = $(top_builddir)/src/libsndfile.la $(OS_SPECIFIC_LINKS) $(ALSA_LIBS)

sndfile_jackplay_SOURCES = sndfile-jackplay.c
sndfile_jackplay_CFLAGS = $(JACK_CFLAGS)
sndfile_jackplay_LDADD = $(top_builddir)/src/libsndfile.la $(JACK_LIBS)

sndfile_convert_SOURCES = sndfile-convert.c common.c common.h
sndfile_convert_LDADD = $(top_builddir)/src/libsndfile.la

sndfile_cmp_SOURCES = sndfile-cmp.c
sndfile_cmp_LDADD = $(top_builddir)/src/libsndfile.la

sndfile_metadata_set_SOURCES = sndfile-metadata-set.c common.c common.h
sndfile_metadata_set_LDADD = $(top_builddir)/src/libsndfile.la

sndfile_metadata_get_SOURCES = sndfile-metadata-get.c
sndfile_metadata_get_LDADD = $(top_builddir)/src/libsndfile.la

