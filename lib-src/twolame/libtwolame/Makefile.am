AM_CFLAGS = -I ../build/ $(WARNING_CFLAGS)

lib_LTLIBRARIES = libtwolame.la
include_HEADERS = twolame.h

libtwolame_la_LDFLAGS  = -export-dynamic -version-info @TWOLAME_SO_VERSION@
libtwolame_la_SOURCES = \
	ath.c \
	ath.h \
	availbits.c \
	availbits.h \
	bitbuffer.c \
	bitbuffer.h \
	bitbuffer_inline.h \
	common.h \
	crc.c \
	crc.h \
	dab.c \
	dab.h \
	encode.c \
	encode.h \
	energy.c \
	energy.h \
	enwindow.h \
	fft.c \
	fft.h \
	get_set.c \
	mem.c \
	mem.h \
	psycho_0.c \
	psycho_0.h \
	psycho_1.c \
	psycho_1.h \
	psycho_1_critband.h \
	psycho_1_freqtable.h \
	psycho_2.c \
	psycho_2.h \
	psycho_2_absthr.h \
	psycho_3.c \
	psycho_3.h \
	psycho_4.c \
	psycho_4.h \
	psycho_n1.c \
	psycho_n1.h \
	subband.c \
	subband.h \
	twolame.c \
	util.c \
	util.h
