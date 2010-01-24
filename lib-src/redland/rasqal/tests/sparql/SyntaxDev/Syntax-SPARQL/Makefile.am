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
syntax-basic-01.rq syntax-basic-02.rq syntax-basic-03.rq		\
syntax-basic-04.rq syntax-basic-05.rq syntax-basic-06.rq		\
syntax-bnodes-01.rq syntax-bnodes-02.rq		\
syntax-bnodes-05.rq syntax-bnodes-06.rq		\
syntax-bnodes-07.rq syntax-expr-01.rq syntax-expr-02.rq			\
syntax-expr-03.rq syntax-expr-04.rq syntax-expr-05.rq			\
syntax-forms-01.rq syntax-forms-02.rq syntax-limit-offset-07.rq		\
syntax-limit-offset-08.rq syntax-limit-offset-09.rq			\
syntax-lists-01.rq syntax-lists-02.rq syntax-lists-03.rq		\
syntax-lists-04.rq syntax-lists-05.rq syntax-lit-01.rq			\
syntax-lit-02.rq syntax-lit-03.rq syntax-lit-04.rq syntax-lit-05.rq	\
syntax-lit-06.rq syntax-lit-07.rq syntax-lit-08.rq syntax-lit-09.rq	\
syntax-lit-10.rq syntax-lit-11.rq syntax-lit-12.rq syntax-lit-13.rq	\
syntax-lit-14.rq syntax-order-01.rq syntax-order-02.rq			\
syntax-order-03.rq syntax-order-04.rq syntax-order-05.rq		\
syntax-order-06.rq syntax-pat-01.rq syntax-pat-02.rq syntax-pat-03.rq	\
syntax-pat-04.rq syntax-qname-01.rq syntax-qname-02.rq			\
syntax-qname-03.rq syntax-qname-04.rq syntax-qname-05.rq		\
syntax-qname-06.rq syntax-qname-07.rq syntax-qname-08.rq		\
syntax-struct-01.rq syntax-struct-02.rq syntax-struct-03.rq		\
syntax-struct-04.rq syntax-struct-05.rq syntax-struct-06.rq		\
syntax-struct-07.rq syntax-struct-08.rq syntax-struct-09.rq		\
syntax-struct-10.rq syntax-struct-11.rq syntax-struct-12.rq		\
syntax-union-01.rq syntax-union-02.rq

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
	set -e; exit $$result

check-sparql-correct: build-roqet $(srcdir)/../../check-sparql
	@set +e; \
	$(ECHO) "Testing SPARQL correct"; \
	PATH=$(top_builddir)/utils:$(top_builddir)@RAPPER_PATH@:$$PATH \
	  $(PERL) $(srcdir)/../../check-sparql -s $(srcdir); \
	result=$$?; \
	if test $$result != 0 ; then \
	    echo "FAILED: $$result failures"; \
	fi; \
	set -e; exit $$result
