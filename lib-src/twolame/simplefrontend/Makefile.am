
AM_CFLAGS = -I../libtwolame/

# This is just sample code - don't install it
noinst_PROGRAMS = @STWOLAME_BIN@
EXTRA_PROGRAMS = stwolame

stwolame_SOURCES = \
	simplefrontend.c \
	audio_wave.h \
	audio_wave.c

stwolame_LDADD = $(top_srcdir)/libtwolame/libtwolame.la

