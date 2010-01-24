# -*- Mode: Makefile -*-
#
# Makefile.am - example automake file for Redland
#

EXAMPLES=example1 example2 example3 example4 example5 example6 example7

OTHER_EXAMPLES=redland_dbus rss2ical rss2atom

EXTRA_PROGRAMS = $(EXAMPLES) $(OTHER_EXAMPLES)

CLEANFILES=$(TESTS) $(EXAMPLES) $(OTHER_EXAMPLES) test*db

examples: $(EXAMPLES)

AM_CPPFLAGS=@CPPFLAGS@ @LIBRDF_CPPFLAGS@ @LIBRDF_INTERNAL_CPPFLAGS@ -I$(top_srcdir)/librdf @LIBRDF_EXTERNAL_CPPFLAGS@
AM_CFLAGS=@CFLAGS@ $(MEM)
AM_LDFLAGS=@LIBRDF_LDFLAGS@ $(top_builddir)/librdf/librdf.la @LIBRDF_EXTERNAL_LIBS@ $(MEM_LIBS)

example1_SOURCES = example1.c

example2_SOURCES = example2.c

example3_SOURCES = example3.c

example4_SOURCES = example4.c

example5_SOURCES = example5.c

example6_SOURCES = example6.c

example7_SOURCES = example7.c

rss2ical_SOURCES = rss2ical.c

rss2atom_SOURCES = rss2atom.c

redland_dbus_SOURCES = redland_dbus.c
redland_dbus_CFLAGS=`pkg-config --cflags dbus-1 glib-2.0 dbus-glib-1 gthread`
redland_dbus_LDFLAGS=`pkg-config --libs dbus-1 glib-2.0 dbus-glib-1 gthread`


$(top_builddir)/raptor/libraptor.la:
	cd $(top_builddir)/raptor && $(MAKE) $(AM_MAKEFLAGS) libraptor.la

$(top_builddir)/rasqal/librasqal.la:
	cd $(top_builddir)/rasqal && $(MAKE) $(AM_MAKEFLAGS) librasqal.la
