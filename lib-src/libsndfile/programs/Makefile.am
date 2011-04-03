## Process this file with automake to produce Makefile.in

bin_PROGRAMS = sndfile-info sndfile-play sndfile-convert sndfile-cmp \
				sndfile-metadata-set sndfile-metadata-get sndfile-interleave \
				sndfile-deinterleave sndfile-concat sndfile-salvage

OS_SPECIFIC_CFLAGS = @OS_SPECIFIC_CFLAGS@
OS_SPECIFIC_LINKS = @OS_SPECIFIC_LINKS@

INCLUDES = -I$(top_srcdir)/src $(OS_SPECIFIC_CFLAGS)

CLEANFILES = *~

# This is the BeOS version of sndfile-play. It needs to be compiled with the C++
# compiler.
EXTRA_DIST = sndfile-play-beos.cpp test-sndfile-metadata-set.py

sndfile_info_SOURCES = sndfile-info.c common.c common.h
sndfile_info_LDADD = $(top_builddir)/src/libsndfile.la

sndfile_play_SOURCES = sndfile-play.c common.c common.h
sndfile_play_LDADD = $(top_builddir)/src/libsndfile.la $(OS_SPECIFIC_LINKS) $(ALSA_LIBS) $(SNDIO_LIBS)

sndfile_convert_SOURCES = sndfile-convert.c common.c common.h
sndfile_convert_LDADD = $(top_builddir)/src/libsndfile.la

sndfile_cmp_SOURCES = sndfile-cmp.c common.c common.h
sndfile_cmp_LDADD = $(top_builddir)/src/libsndfile.la

sndfile_metadata_set_SOURCES = sndfile-metadata-set.c common.c common.h
sndfile_metadata_set_LDADD = $(top_builddir)/src/libsndfile.la

sndfile_metadata_get_SOURCES = sndfile-metadata-get.c common.c common.h
sndfile_metadata_get_LDADD = $(top_builddir)/src/libsndfile.la

sndfile_interleave_SOURCES = sndfile-interleave.c common.c common.h
sndfile_interleave_LDADD = $(top_builddir)/src/libsndfile.la

sndfile_deinterleave_SOURCES = sndfile-deinterleave.c common.c common.h
sndfile_deinterleave_LDADD = $(top_builddir)/src/libsndfile.la

sndfile_concat_SOURCES = sndfile-concat.c common.c common.h
sndfile_concat_LDADD = $(top_builddir)/src/libsndfile.la

sndfile_salvage_SOURCES = sndfile-salvage.c common.c common.h
sndfile_salvage_LDADD = $(top_builddir)/src/libsndfile.la

