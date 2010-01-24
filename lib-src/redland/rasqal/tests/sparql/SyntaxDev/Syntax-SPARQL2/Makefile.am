# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Rasqal SPARQL tests
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

SPARQL_MANIFEST_FILES= manifest.ttl

SPARQL_TEST_FILES= \
syntax-bnode-01.rq syntax-bnode-02.rq syntax-bnode-03.rq		\
syntax-dataset-01.rq syntax-dataset-02.rq syntax-dataset-03.rq		\
syntax-dataset-04.rq syntax-esc-01.rq syntax-esc-02.rq			\
syntax-esc-03.rq syntax-esc-04.rq syntax-form-ask-01.rq			\
syntax-form-ask-02.rq syntax-form-construct01.rq			\
syntax-form-construct02.rq syntax-form-construct03.rq			\
syntax-form-construct04.rq syntax-form-construct05.rq			\
syntax-form-construct06.rq syntax-form-describe01.rq			\
syntax-form-describe02.rq syntax-form-select-01.rq			\
syntax-form-select-02.rq syntax-function-01.rq syntax-function-02.rq	\
syntax-function-03.rq syntax-function-04.rq syntax-general-01.rq	\
syntax-general-02.rq syntax-general-03.rq syntax-graph-01.rq		\
syntax-graph-03.rq syntax-graph-04.rq		\
syntax-graph-05.rq syntax-keywords-01.rq		\
syntax-keywords-02.rq syntax-keywords-03.rq syntax-lists-01.rq		\
syntax-lists-02.rq syntax-lists-03.rq syntax-lists-04.rq		\
syntax-lists-05.rq

# Test failures
EXPECTED_SPARQL_PARSER_FAILURES=

EXPECTED_SPARQL_PARSER_FAILURES_COUNT=0

EXPECTED_SPARQL_CORRECT_FAILURES= \
$(EXPECTED_SPARQL_PARSER_FAILURES)

EXTRA_DIST= \
$(SPARQL_MANIFEST_FILES) \
$(SPARQL_TEST_FILES)


build-roqet:
	@(cd $(top_builddir)/utils ; $(MAKE) roqet)

build-sparql-lexer-test:
	@(cd $(top_builddir)/src ; $(MAKE) sparql_lexer_test)

build-sparql-parser-test:
	@(cd $(top_builddir)/src ; $(MAKE) sparql_parser_test)


check-local: check-sparql-lexer check-sparql-parser \
check-sparql-correct

check-sparql-lexer: build-sparql-lexer-test
	@set +e; result=0; \
	$(ECHO) "Testing legal SPARQL lexing"; \
	for test in $(SPARQL_TEST_FILES); do \
	  $(ECHO) $(ECHO_N) "Checking $$test $(ECHO_C)"; \
	  $(top_builddir)/src/sparql_lexer_test $(srcdir)/$$test >/dev/null; \
	  status=$$?; \
	  if test $$status != 0 -a $$status != 2 ; then \
	    $(ECHO) FAILED returned status $$status; result=1; \
	  else \
	    $(ECHO) ok; \
	  fi; \
	done; \
	set -e; exit $$result

check-sparql-parser: build-sparql-parser-test
	@set +e; result=0; \
	$(ECHO) "Testing legal SPARQL parsing"; \
	for test in $(SPARQL_TEST_FILES); do \
	  $(ECHO) $(ECHO_N) "Checking $$test $(ECHO_C)"; \
	  $(top_builddir)/src/sparql_parser_test $(srcdir)/$$test >/dev/null; \
	  status=$$?; \
	  if test $$status != 0 -a $$status != 2 ; then \
	    $(ECHO) FAILED returned status $$status; result=1; \
	    fails="$$fails $$test"; \
	  else \
	    $(ECHO) ok; \
	  fi; \
	done; \
	if test $$result != 0 ; then \
	  echo "Expecting test failures: $(EXPECTED_SPARQL_PARSER_FAILURES)"; \
	  result=`echo $$fails | wc -w`; \
	  if test $$result = $(EXPECTED_SPARQL_PARSER_FAILURES_COUNT) ; then \
	    echo "OK: Ignoring $$result expected test failures"; result=0; \
          else \
	    echo "FAILED: $$result failures $$fails does not match expected $(EXPECTED_SPARQL_PARSER_FAILURES_COUNT)"; result=1; \
          fi; \
	fi; \
	set -e; exit $$result

check-sparql-correct: build-roqet $(srcdir)/../../check-sparql
	@set +e; \
	expect=`echo $(EXPECTED_SPARQL_CORRECT_FAILURES) | wc -w`; \
	$(ECHO) "Testing SPARQL correct"; \
	PATH=$(top_builddir)/utils:$(top_builddir)@RAPPER_PATH@:$$PATH \
	  $(PERL) $(srcdir)/../../check-sparql -s $(srcdir); \
	result=$$?; \
	if test $$result != 0 ; then \
	  echo "Expecting test failures: $(EXPECTED_SPARQL_CORRECT_FAILURES)"; \
	  if test $$result = $$expect ; then \
	    echo "OK: Ignoring $$result expected test failures"; result=0; \
          else \
	    echo "FAILED: $$result failures does not match expected $$expect"; result=1; \
	    fails="$$fails $$test"; \
          fi; \
	fi; \
	if test $$result != 0 ; then \
	  echo "FAILED tests:$$fails"; \
	fi; \
	set -e; exit $$result
