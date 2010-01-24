# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Raptor TRiG tests
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

TEST_FILES=\
example1.trig example2.trig example3.trig

TEST_OUT_FILES=\
example1.out example2.out example3.out

# Used to make N-triples output consistent
BASE_URI=http://example.librdf.org/

EXTRA_DIST = \
	$(TEST_FILES) \
	$(TEST_OUT_FILES)

build-rapper:
	@(cd $(top_builddir)/utils ; $(MAKE) rapper)

check-local: check-trig

check-trig: build-rapper $(TEST_FILES)
	@result=0; \
	$(ECHO) "Testing legal TRiG"; \
	for test in $(TEST_FILES); do \
	  name=`basename $$test .trig` ; \
	  baseuri=$(BASE_URI)$$test; \
	  $(ECHO) $(ECHO_N) "Checking $$test $(ECHO_C)"; \
	  $(top_builddir)/utils/rapper -q -i trig -o ntriples $(srcdir)/$$test $$baseuri > $$name.res 2> $$name.err; \
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
	  rm -f $$name.res $$name.err; \
	done; \
	exit $$result
