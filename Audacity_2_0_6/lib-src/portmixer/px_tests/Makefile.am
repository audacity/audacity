check_PROGRAMS = px_test

px_test_CPPFLAGS = -I$(top_srcdir)/include $(PORTAUDIO_CFLAGS)
px_test_LDADD = $(top_builddir)/src/libportmixer.la $(PORTAUDIO_LIBS)
px_test_SOURCES = px_test.c
