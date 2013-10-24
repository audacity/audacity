## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(top_srcdir)/include @OGG_CFLAGS@

check_PROGRAMS = test

check: $(check_PROGRAMS)
	./test$(EXEEXT)

test_SOURCES = util.c util.h write_read.c write_read.h test.c
test_LDADD = ../lib/libvorbisenc.la ../lib/libvorbis.la @OGG_LIBS@

debug:
	$(MAKE) all CFLAGS="@DEBUG@"

profile:
	$(MAKE) all CFLAGS="@PROFILE@"
