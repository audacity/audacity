
AM_CFLAGS = -I$(top_srcdir)/libtwolame/ $(WARNING_CFLAGS)

# This is just sample code - don't install it
noinst_PROGRAMS = stwolame

stwolame_SOURCES = \
	simplefrontend.c \
	audio_wave.h \
	audio_wave.c

stwolame_LDADD = $(top_builddir)/libtwolame/libtwolame.la

