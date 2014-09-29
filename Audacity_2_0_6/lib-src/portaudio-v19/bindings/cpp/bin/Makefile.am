BINDIR = $(top_srcdir)/example
LIBDIR = $(top_builddir)/lib

noinst_PROGRAMS = devs sine

LDADD = $(LIBDIR)/libportaudiocpp.la $(top_builddir)/$(PORTAUDIO_ROOT)/lib/libportaudio.la

devs_SOURCES = $(BINDIR)/devs.cxx
sine_SOURCES = $(BINDIR)/sine.cxx
