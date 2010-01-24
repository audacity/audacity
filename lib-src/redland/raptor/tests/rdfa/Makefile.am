# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Raptor RDFA tests
#
# Copyright (C) 2008, David Beckett http://purl.org/net/dajobe/
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

BASE_URI=http://www.w3.org/2006/07/SWD/RDFa/testsuite/xhtml1-testcases/

TEST_FILES= 0001.xhtml 0006.xhtml 0007.xhtml 0008.xhtml 0009.xhtml	\
0010.xhtml 0011.xhtml 0012.xhtml 0013.xhtml 0014.xhtml 0015.xhtml	\
0017.xhtml 0018.xhtml 0019.xhtml 0020.xhtml 0021.xhtml 0023.xhtml	\
0025.xhtml 0026.xhtml 0027.xhtml 0029.xhtml 0030.xhtml 0031.xhtml	\
0032.xhtml 0033.xhtml 0034.xhtml 0035.xhtml 0036.xhtml 0037.xhtml	\
0038.xhtml 0039.xhtml 0040.xhtml 0041.xhtml 0042.xhtml 0046.xhtml	\
0047.xhtml 0048.xhtml 0049.xhtml 0050.xhtml 0051.xhtml 0052.xhtml	\
0053.xhtml 0054.xhtml 0055.xhtml 0056.xhtml 0057.xhtml 0058.xhtml	\
0059.xhtml 0060.xhtml 0061.xhtml 0062.xhtml 0063.xhtml 0064.xhtml	\
0065.xhtml 0066.xhtml 0067.xhtml 0068.xhtml 0069.xhtml 0070.xhtml	\
0071.xhtml 0072.xhtml 0073.xhtml 0074.xhtml 0075.xhtml 0076.xhtml	\
0077.xhtml 0078.xhtml 0079.xhtml 0080.xhtml 0081.xhtml 0082.xhtml	\
0083.xhtml 0084.xhtml 0085.xhtml 0086.xhtml 0087.xhtml 0088.xhtml	\
0089.xhtml 0090.xhtml 0091.xhtml 0092.xhtml 0093.xhtml 0094.xhtml	\
0099.xhtml 0100.xhtml 0101.xhtml 0102.xhtml 0103.xhtml 0104.xhtml	\
0105.xhtml 0106.xhtml 0107.xhtml 0108.xhtml 0109.xhtml 0110.xhtml	\
0111.xhtml 0112.xhtml 1001.xhtml

TEST_OUT_FILES = 0001.out 0006.out 0007.out 0008.out 0009.out	\
0010.out 0011.out 0012.out 0013.out 0014.out 0015.out 0017.out	\
0018.out 0019.out 0020.out 0021.out 0023.out 0025.out 0026.out	\
0027.out 0029.out 0030.out 0031.out 0032.out 0033.out 0034.out	\
0035.out 0036.out 0037.out 0038.out 0039.out 0040.out 0041.out	\
0042.out 0046.out 0047.out 0048.out 0049.out 0050.out 0051.out	\
0052.out 0053.out 0054.out 0055.out 0056.out 0057.out 0058.out	\
0059.out 0060.out 0061.out 0062.out 0063.out 0064.out 0065.out	\
0066.out 0067.out 0068.out 0069.out 0070.out 0071.out 0072.out	\
0073.out 0074.out 0075.out 0076.out 0077.out 0078.out 0079.out	\
0080.out 0081.out 0082.out 0083.out 0084.out 0085.out 0086.out	\
0087.out 0088.out 0089.out 0090.out 0091.out 0092.out 0093.out	\
0094.out 0099.out 0100.out 0101.out 0102.out 0103.out 0104.out	\
0105.out 0106.out 0107.out 0108.out 0109.out 0110.out 0111.out	\
0112.out 1001.out

TEST_BAD_FILES=

ALL_TEST_FILES= \
	$(TEST_FILES) \
	$(TEST_BAD_FILES) \
	$(TEST_OUT_FILES)

# Expected test failures:
#   0094.xhtml - XML namespaces are in a different order to expected output
#   0101.xhtml - XML namespaces are in a different order to expected output
#   0102.xhtml - XML namespaces are in a different order to expected output
#   0103.xhtml - XML namespaces are in a different order to expected output
# (Add a space to the start of this string)
EXPECTED_FAILURES=" 0094.xhtml 0101.xhtml 0102.xhtml 0103.xhtml"

EXTRA_DIST = $(ALL_TEST_FILES)

build-rapper:
	@(cd $(top_builddir)/utils ; $(MAKE) rapper$(EXEEXT))

build-rdfdiff:
	@(cd $(top_builddir)/utils ; $(MAKE) rdfdiff$(EXEEXT))

check-local:
@RAPTOR_PARSER_RDFA_TRUE@check-local: check-rdfa

check-rdfa: build-rdfdiff build-rapper $(TEST_FILES)
	@result=0; failures=; failure_count=0; \
	$(ECHO) "Testing RDFA"; \
	for test in $(TEST_FILES); do \
	  name=`basename $$test .xhtml` ; \
	  $(ECHO) $(ECHO_N) "Checking $$test $(ECHO_C)"; \
	  baseuri=$(BASE_URI)$$test; \
	  $(top_builddir)/utils/rapper -f noNet -q -i rdfa -I $$baseuri -o ntriples $(srcdir)/$$test > $$name-res.nt 2> $$name.err; \
	  status1=$$?; \
	  $(top_builddir)/utils/rdfdiff -f ntriples -u $$baseuri -t ntriples $(srcdir)/$$name.out $$name-res.nt > $$name.res 2> $$name.err; \
	  status2=$$?; \
	  if test $$status1 = 0 -a $$status2 = 0; then \
	    $(ECHO) "ok"; \
	  else \
	    $(ECHO) "FAILED"; \
	    $(ECHO) "Triple counts: expected: `wc -l < $(srcdir)/$$name.out` got: `wc -l < $$name-res.nt` "; \
	    $(ECHO) $(top_builddir)/utils/rapper -f noNet -q -i rdfa -I $$baseuri -o ntriples $(srcdir)/$$test '>' $$name-res.nt; \
	    $(ECHO) $(top_builddir)/utils/rdfdiff -f ntriples -u $$baseuri -t ntriples $(srcdir)/$$name.out $$name-res.nt '>' $$name.res; \
	    cat $$name.err $$name.res; \
	    failures="$$failures $$test"; \
	    failure_count=`expr $$failure_count + 1`; \
	  fi; \
	  rm -f $$name-res.nt $$name.res $$name.err; \
	done; \
	if test "X$$failures" != X; then \
	  echo "$$failure_count tests FAILED: $$failures"; \
	  if test "$$failures" = $(EXPECTED_FAILURES); then \
	    echo "Ignoring expected failures"; \
	    result=0; \
	  fi; \
	fi; \
	exit $$result
