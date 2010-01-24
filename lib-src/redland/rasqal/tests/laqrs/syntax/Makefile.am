# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Rasqal syntax LAQRS tests
#
# Copyright (C) 2007-2008, David Beckett http://www.dajobe.org/
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

LAQRS_MODEL_FILES= \
data.n3

LAQRS_TEST_FILES= \
count1.rq \
count2.rq \
count3.rq \
count4.rq \
coverage.rq \
explain.rq \
delete.rq \
insert.rq

LAQRS_BAD_TEST_FILES= \
bad-select-expr.rq

EXPECTED_LAQRS_CORRECT_FAILURES=

EXTRA_DIST= \
$(LAQRS_MODEL_FILES) \
$(LAQRS_TEST_FILES) \
$(LAQRS_BAD_TEST_FILES)

build-sparql-parser-test:
	@(cd $(top_builddir)/src ; $(MAKE) sparql_parser_test)

check-local: check-laqrs-parser check-bad-laqrs

check-laqrs-parser: build-sparql-parser-test
	@set +e; result=0; \
	$(ECHO) "Testing legal LAQRS parsing"; \
	for test in $(LAQRS_TEST_FILES); do \
	  $(ECHO) $(ECHO_N) "Checking $$test $(ECHO_C)"; \
	  $(top_builddir)/src/sparql_parser_test -i laqrs $(srcdir)/$$test >/dev/null; \
	  status=$$?; \
	  if test $$status != 0 -a $$status != 2 ; then \
	    $(ECHO) FAILED returned status $$status; result=1; \
	    fails="$$fails $$test"; \
	  else \
	    $(ECHO) ok; \
	  fi; \
	done; \
	if test $$result != 0 ; then \
	  echo "FAILED tests:$$fails"; \
	fi; \
	set -e; exit $$result

check-bad-laqrs: build-sparql-parser-test
	@set +e; result=0; \
	$(ECHO) "Testing bad LAQRS fails"; \
	for test in $(LAQRS_BAD_TEST_FILES); do \
	  $(ECHO) $(ECHO_N) "Checking $$test $(ECHO_C)"; \
	  $(top_builddir)/src/sparql_parser_test -i laqrs $(srcdir)/$$test >$$name.res 2> $$name.err; \
	  status=$$?; \
	  if test $$status -eq 1 ; then \
	    $(ECHO) "ok"; \
	  elif test $$status -eq 2; then \
	    $(ECHO) "FAILED - parsing succeeded with a warning"; result=1; \
	    fails="$$fails $$test"; \
	  elif test $$status -eq 0 ; then \
	    $(ECHO) "FAILED - parsing succeeded but should have failed"; \
	    fails="$$fails $$test"; \
	    cat $$name.res; result=1; \
	  else \
	    $(ECHO) "FAILED - parsing failed with unknown status $$status"; \
	    fails="$$fails $$test"; \
	    cat $$name.res; result=1; \
	  fi; \
	  rm -f $$name.res $$name.err ; \
	done; \
	if test $$result != 0 ; then \
	  echo "FAILED tests:$$fails"; \
	fi; \
	set -e; exit $$result
