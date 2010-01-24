EXTRA_DIST = ladspa.h

AM_CFLAGS = -std=c99 -I$(top_srcdir) @REDLAND_CFLAGS@ @SLV2_CFLAGS@ @LV2CORE_CFLAGS@

bin_PROGRAMS = lv2_list lv2_inspect
noinst_PROGRAMS = ladspa2lv2

lv2_list_SOURCES = lv2_list.c
lv2_list_LDADD = ../src/libslv2.la 

lv2_inspect_SOURCES = lv2_inspect.c
lv2_inspect_LDADD = ../src/libslv2.la 

ladspa2lv2_SOURCES = ladspa2lv2.c
ladspa2lv2_LDADD = -ldl -lm @REDLAND_LIBS@

