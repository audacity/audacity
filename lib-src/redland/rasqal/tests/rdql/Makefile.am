# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Rasqal RDQL tests
#
# Copyright (C) 2004-2008, David Beckett http://www.dajobe.org/
# Copyright (C) 2004-2004, University of Bristol, UK http://www.bristol.ac.uk/
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

SUBDIRS=testsuite

RDQL_TEST_FILES= \
example1.rdql example2.rdql example3.rdql example4.rdql example5.rdql \
example_at_1.rdql example_at_2.rdql example_at_3.rdql example_at_4.rdql \
example_at_5.rdql example_at_6.rdql example_at_7.rdql \
note-ex1.rdql note-ex2.rdql note-ex3.rdql note-ex4.rdql


EXTRA_DIST = \
$(RDQL_TEST_FILES) \
dc.rdf

build-rdql-lexer-test:
	@(cd $(top_builddir)/src ; $(MAKE) rdql_lexer_test)

build-rdql-parser-test:
	@(cd $(top_builddir)/src ; $(MAKE) rdql_parser_test)

check-local: check-rdql-lexer check-rdql-parser

check-rdql-lexer: build-rdql-lexer-test
	@set +e; result=0; \
	$(ECHO) "Testing legal RDQL lexing"; \
	for test in $(RDQL_TEST_FILES); do \
	  $(ECHO) $(ECHO_N) "Checking $$test $(ECHO_C)"; \
	  $(top_builddir)/src/rdql_lexer_test $(srcdir)/$$test >/dev/null; \
	  status=$$?; \
	  if test $$status != 0 -a $$status != 2 ; then \
	    $(ECHO) FAILED returned status $$status; result=1; \
	  else \
	    $(ECHO) ok; \
	  fi; \
	done; \
	set -e; exit $$result

check-rdql-parser: build-rdql-parser-test
	@set +e; result=0; \
	$(ECHO) "Testing legal RDQL parsing"; \
	for test in $(RDQL_TEST_FILES); do \
	  $(ECHO) $(ECHO_N) "Checking $$test $(ECHO_C)"; \
	  $(top_builddir)/src/rdql_parser_test $(srcdir)/$$test >/dev/null; \
	  status=$$?; \
	  if test $$status != 0 -a $$status != 2 ; then \
	    $(ECHO) FAILED returned status $$status; result=1; \
	  else \
	    $(ECHO) ok; \
	  fi; \
	done; \
	set -e; exit $$result
