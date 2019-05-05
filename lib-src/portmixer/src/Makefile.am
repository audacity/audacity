lib_LTLIBRARIES = libportmixer.la

libportmixer_la_CPPFLAGS = -I$(top_srcdir)/include $(PORTAUDIO_CFLAGS)
libportmixer_la_SOURCES = \
	px_mixer.c \
	px_mixer.h \
	$(NULL)
libportmixer_la_LIBADD = $(PORTAUDIO_LIBS)

if USE_UNIX_OSS
libportmixer_la_SOURCES += px_unix_oss.c
endif

if USE_LINUX_ALSA
libportmixer_la_SOURCES += px_linux_alsa.c
endif

if USE_MAC_COREAUDIO
libportmixer_la_SOURCES += px_mac_coreaudio.c
endif

if USE_WIN_WMME
libportmixer_la_SOURCES += px_win_wmme.c
libportmixer_la_LIBADD += -lwinmm
endif

if USE_WIN_DS
libportmixer_la_SOURCES += px_win_ds.c
libportmixer_la_LIBADD += -ldsound
endif

if USE_WIN_COMMON
libportmixer_la_SOURCES += px_win_common.c px_win_endpoint.c
libportmixer_la_LIBADD += -lole32
endif

EXTRA_DIST = \
	px_example_api.c \
	px_win_common.h \
	px_win_endpoint.h \
	px_win_wasapi.c \
	$(NULL)
