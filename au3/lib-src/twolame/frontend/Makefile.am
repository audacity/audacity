
AM_CFLAGS = -I$(top_srcdir)/build/ -I$(top_srcdir)/libtwolame/ $(SNDFILE_CFLAGS) $(WARNING_CFLAGS)
AM_LDFLAGS = $(SNDFILE_LIBS)

bin_PROGRAMS = @TWOLAME_BIN@
EXTRA_PROGRAMS = twolame

twolame_SOURCES = frontend.c frontend.h audioin_raw.c audioin_sndfile.c
twolame_LDADD = $(top_builddir)/libtwolame/libtwolame.la
