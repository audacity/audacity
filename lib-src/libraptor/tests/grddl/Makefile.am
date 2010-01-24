# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Raptor GRDDL tests
#
# Copyright (C) 2007, David Beckett http://purl.org/net/dajobe/
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

TEST_FILES=test-01.html
TEST_BAD_FILES=
TEST_OUT_FILES=test-01.out
TEST_DATA_FILES=\
data-01.rdf data-02.rdf data-01.nt

ALL_TEST_FILES= \
	$(TEST_FILES) \
	$(TEST_BAD_FILES) \
	$(TEST_OUT_FILES) \
	$(TEST_DATA_FILES)

EXTRA_DIST = $(ALL_TEST_FILES)

build-rapper:
	@(cd $(top_builddir)/utils ; $(MAKE) rapper$(EXEEXT))

check-local:
@RAPTOR_PARSER_GRDDL_TRUE@check-local: check-grddl

check-grddl: build-rapper $(TEST_FILES)
	@result=0; \
	$(ECHO) "Testing GRDDL"; \
	for test in $(TEST_FILES); do \
	  name=`basename $$test .html` ; \
	  $(ECHO) $(ECHO_N) "Checking $$test $(ECHO_C)"; \
	  $(top_builddir)/utils/rapper -f noNet -q -i grddl -o ntriples $(srcdir)/$$test > $$name.res 2> $$name.err; \
	  status=$$?; \
	  if test $$status != 0 -a $$status != 2 ; then \
	    $(ECHO) FAILED returned status $$status; result=1; \
	  elif cmp $(srcdir)/$$name.out $$name.res >/dev/null 2>&1; then \
	    if test $$status = 2 ; then \
	      $(ECHO) "ok with warnings"; grep Warning $$name.err; \
	    else \
	      $(ECHO) "ok"; \
	    fi; \
	  else \
	    $(ECHO) "FAILED"; \
	    diff $(srcdir)/$$name.out $$name.res; result=1; \
	  fi; \
	  if test $$status != 1 ; then \
	    rm -f $$name.res $$name.err; \
	  fi; \
	done; \
	exit $$result
