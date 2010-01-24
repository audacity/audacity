# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Raptor utils
#
# Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
# Copyright (C) 2000-2004, University of Bristol, UK http://www.bristol.ac.uk/
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
# 


bin_PROGRAMS = rapper
EXTRA_PROGRAMS = rdfdiff

man_MANS = rapper.1

CLEANFILES=rdfdiff

# Memory debugging alternatives
MEM=@MEM@
MEM_LIBS=@MEM_LIBS@

# 1) None (use standard functions directly)
#MEM=
#MEM_LIBS=

# 2) Use dmalloc library
#MEM=-DRAPTOR_MEMORY_DEBUG_DMALLOC=1
#MEM_LIBS=-ldmalloc

AM_CPPFLAGS= -I$(top_srcdir)/src 
AM_CFLAGS= $(MEM)
STANDARD_CFLAGS= @STANDARD_CFLAGS@ $(MEM)
LIBS=@LIBS@ $(MEM_LIBS)


EXTRA_DIST= \
rapper.html \
$(man_MANS)

rapper_SOURCES = rapper.c
if GETOPT
rapper_SOURCES += getopt.c raptor_getopt.h
endif
rapper_LDADD= $(top_builddir)/src/libraptor.la

rdfdiff_SOURCES = rdfdiff.c
if GETOPT
rdfdiff_SOURCES += getopt.c raptor_getopt.h
endif
rdfdiff_LDADD= $(top_builddir)/src/libraptor.la


if MAINTAINER_MODE
rapper.html: $(srcdir)/rapper.1 $(srcdir)/../fix-groff-xhtml
	-groff -man -Thtml -P-l $< | tidy -asxml -wrap 1000 2>/dev/null | $(PERL) $(srcdir)/../fix-groff-xhtml $@
endif

$(top_builddir)/src/libraptor.la:
	cd $(top_builddir)/src && $(MAKE) libraptor.la
