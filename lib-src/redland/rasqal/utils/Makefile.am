# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Rasqal utilities
#
# Copyright (C) 2005-2008, David Beckett http://www.dajobe.org/
# Copyright (C) 2005-2005, University of Bristol, UK http://www.bristol.ac.uk/
# 
# This package is Free Software and part of Redland http://librdf.org/
# 
# It is licensed under the following three licenses as alternatives:
#   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
#   2. GNU General Public License (GPL) V2 or any newer version
#   3. Apache License, V2.0 or any newer version
# 
# You may not use this file except in compliance with at least one of
# the above three licenses.
# 
# See LICENSE.html or LICENSE.txt at the top of this package for the
# complete terms and further detail along with the license texts for
# the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
# 

bin_PROGRAMS = roqet
EXTRA_PROGRAMS = srxread

CLEANFILES = $(EXTRA_PROGRAMS)

man_MANS = roqet.1

AM_CPPFLAGS= @RASQAL_INTERNAL_CPPFLAGS@ -I$(top_srcdir)/src
AM_CFLAGS = $(MEM)
STANDARD_CFLAGS= @STANDARD_CFLAGS@ $(MEM)
LIBS= @RASQAL_INTERNAL_LIBS@ $(MEM_LIBS)

roqet_SOURCES = roqet.c
if GETOPT
roqet_SOURCES += getopt.c rasqal_getopt.h
endif
roqet_LDADD = $(top_builddir)/src/librasqal.la

srxread_SOURCES = srxread.c
srxread_LDADD = $(top_builddir)/src/librasqal.la

EXTRA_DIST= \
$(man_MANS) \
roqet.html

if MAINTAINER_MODE
roqet.html: $(srcdir)/roqet.1 $(srcdir)/../fix-groff-xhtml
	-groff -man -Thtml -P-l $< | tidy -asxml -wrap 1000 2>/dev/null | $(PERL) $(srcdir)/../fix-groff-xhtml $@

endif

$(top_builddir)/src/librasqal.la:
	cd $(top_builddir)/src && $(MAKE) librasqal.la
