AM_CFLAGS = -std=c99 -I$(top_srcdir) @REDLAND_CFLAGS@ @SLV2_CFLAGS@

if WITH_JACK

noinst_HEADERS = lv2_event.h lv2_event_helpers.h lv2_uri_map.h

bin_PROGRAMS = lv2_jack_host lv2_simple_jack_host

lv2_jack_host_CFLAGS = @JACK_CFLAGS@ $(AM_CFLAGS)

lv2_jack_host_DEPENDENCIES = ../src/libslv2.la
lv2_jack_host_LDADD        = ../src/libslv2.la @JACK_LIBS@ @REDLAND_LIBS@

lv2_simple_jack_host_LDADD = ../src/libslv2.la @JACK_LIBS@ @REDLAND_LIBS@

lv2_jack_host_SOURCES = \
	jack_compat.h \
	lv2_jack_host.c

lv2_simple_jack_host_SOURCES = \
	jack_compat.h \
	lv2_simple_jack_host.c

endif

