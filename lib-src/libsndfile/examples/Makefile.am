## Process this file with automake to produce Makefile.in

noinst_PROGRAMS = make_sine sfprocess list_formats generate sndfilehandle sndfile-to-text

INCLUDES = -I$(top_srcdir)/src

sndfile_to_text_SOURCES = sndfile-to-text.c
sndfile_to_text_LDADD = $(top_builddir)/src/libsndfile.la

make_sine_SOURCES = make_sine.c
make_sine_LDADD = $(top_builddir)/src/libsndfile.la

sfprocess_SOURCES = sfprocess.c
sfprocess_LDADD = $(top_builddir)/src/libsndfile.la

list_formats_SOURCES = list_formats.c
list_formats_LDADD = $(top_builddir)/src/libsndfile.la

generate_SOURCES = generate.c
generate_LDADD = $(top_builddir)/src/libsndfile.la

sndfilehandle_SOURCES = sndfilehandle.cc
sndfilehandle_LDADD = $(top_builddir)/src/libsndfile.la

