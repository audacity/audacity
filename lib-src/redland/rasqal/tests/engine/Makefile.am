# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Rasqal query engine tests
#
# Copyright (C) 2004-2008, David Beckett http://www.dajobe.org/
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

local_tests=rasqal_order_test$(EXEEXT) rasqal_graph_test$(EXEEXT)

EXTRA_PROGRAMS=$(local_tests)

AM_CPPFLAGS=@RASQAL_INTERNAL_CPPFLAGS@ -I$(top_srcdir)/src
AM_CFLAGS=@RASQAL_INTERNAL_CPPFLAGS@ $(MEM)
AM_LDFLAGS=@RASQAL_INTERNAL_LIBS@ @RASQAL_EXTERNAL_LIBS@ $(MEM_LIBS)

CLEANFILES=$(local_tests)

rasqal_order_test_SOURCES = rasqal_order_test.c
rasqal_order_test_LDADD = $(top_builddir)/src/librasqal.la

rasqal_graph_test_SOURCES = rasqal_graph_test.c
rasqal_graph_test_LDADD = $(top_builddir)/src/librasqal.la


# These are compiled here and used elsewhere for running tests
check-local: $(local_tests) run-rasqal-tests

run-rasqal-tests: rasqal_order_test rasqal_graph_test
	@tests="rasqal_order_test rasqal_graph_test"; \
	status=0; \
	for tst in $$tests; do \
	  if test -f ./$$tst; then dir=./; \
	  elif test -f $$tst; then dir=; \
	  else dir="$(top_srcdir)/"; fi; \
	  if $${dir}$$tst $(top_srcdir)/data/; then \
	    echo "PASS: $$tst"; \
	  else \
	    echo "FAIL: $$tst"; \
	    status=1; \
	    break; \
	  fi; \
	done; \
	exit $$status


$(top_builddir)/src/librasqal.la:
	cd $(top_builddir)/src && $(MAKE) librasqal.la

$(top_builddir)/../raptor/src/libraptor.la:
	cd $(top_builddir)/../raptor/src && $(MAKE) $(AM_MAKEFLAGS) libraptor.la
