# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Raptor libraptor
#
# Copyright (C) 2000-2006, David Beckett http://purl.org/net/dajobe/
# Copyright (C) 2000-2005, University of Bristol, UK http://www.bristol.ac.uk/
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


bin_SCRIPTS = raptor-config

lib_LTLIBRARIES = libraptor.la

include_HEADERS = raptor.h

noinst_HEADERS = raptor_internal.h win32_raptor_config.h

man_MANS = raptor-config.1

TESTS=raptor_parse_test raptor_rfc2396_test raptor_uri_test \
raptor_namespace_test strcasecmp_test raptor_www_test \
raptor_sequence_test raptor_stringbuffer_test \
raptor_uri_win32_test raptor_iostream_test raptor_xml_writer_test \
raptor_turtle_writer_test raptor_avltree_test
if RAPTOR_PARSER_RDFXML
TESTS += raptor_set_test raptor_xml_test
endif

CLEANFILES=$(TESTS) \
turtle_lexer_test turtle_parser_test \
n3_lexer_test n3_parser_test \
rdfdiff

MAINTAINERCLEANFILES=turtle_lexer.c turtle_lexer.h \
turtle_parser.c turtle_parser.h turtle_parser.output \
n3_lexer.c n3_lexer.h \
n3_parser.c n3_parser.h n3_parser.output

# Memory debugging alternatives
MEM=@MEM@
MEM_LIBS=@MEM_LIBS@

# 1) None (use standard functions directly)
#MEM=
#MEM_LIBS=

# 2) Use dmalloc library
#MEM=-DRAPTOR_MEMORY_DEBUG_DMALLOC=1
#MEM_LIBS=-ldmalloc

AM_CFLAGS=$(MEM)
STANDARD_CFLAGS=@STANDARD_CFLAGS@ $(MEM)
LIBS=@LIBS@ $(MEM_LIBS)


libraptor_la_SOURCES = raptor_parse.c raptor_serialize.c \
raptor_rfc2396.c raptor_uri.c raptor_locator.c \
raptor_namespace.c raptor_qname.c \
raptor_feature.c raptor_general.c raptor_utf8.c \
raptor_www.c \
raptor_identifier.c raptor_statement.c \
raptor_sequence.c raptor_stringbuffer.c raptor_iostream.c \
raptor_xml.c raptor_xml_writer.c raptor_set.c turtle_common.c \
raptor_turtle_writer.c raptor_xsd.c raptor_avltree.c snprintf.c \
raptor_json_writer.c
if RAPTOR_PARSER_RDFXML
if RAPTOR_XML_LIBXML
libraptor_la_SOURCES += raptor_libxml.c
endif
if RAPTOR_XML_EXPAT
libraptor_la_SOURCES += raptor_expat.c
endif
libraptor_la_SOURCES += raptor_rdfxml.c raptor_sax2.c
endif
if RAPTOR_PARSER_TURTLE
libraptor_la_SOURCES += turtle_lexer.c turtle_lexer.h turtle_parser.c turtle_parser.h turtle_common.h
else
if RAPTOR_PARSER_TRIG
libraptor_la_SOURCES += turtle_lexer.c turtle_lexer.h turtle_parser.c turtle_parser.h turtle_common.h
endif
endif
if RAPTOR_PARSER_N3
libraptor_la_SOURCES += n3_lexer.c n3_lexer.h n3_parser.c n3_parser.h n3_common.h
endif
if RAPTOR_PARSER_NTRIPLES
libraptor_la_SOURCES += ntriples_parse.c
endif
if RAPTOR_RSS_COMMON
libraptor_la_SOURCES += raptor_rss_common.c raptor_rss.h
endif
if RAPTOR_PARSER_RSS
libraptor_la_SOURCES += raptor_rss.c
endif
if RAPTOR_PARSER_GRDDL
libraptor_la_SOURCES += raptor_grddl.c
endif
if RAPTOR_PARSER_GUESS
libraptor_la_SOURCES += raptor_guess.c
endif
# Always available
libraptor_la_SOURCES += raptor_serialize_simple.c
if RAPTOR_SERIALIZER_RDFXML
libraptor_la_SOURCES += raptor_serialize_rdfxml.c
endif
if RAPTOR_SERIALIZER_NTRIPLES
libraptor_la_SOURCES += raptor_serialize_ntriples.c
endif

#raptor_abbrev.c required by both turtle and xml-abbrev
if RAPTOR_SERIALIZER_RDFXML_ABBREV
libraptor_la_SOURCES += raptor_abbrev.c
else
if RAPTOR_SERIALIZER_TURTLE
libraptor_la_SOURCES += raptor_abbrev.c
endif
endif

if RAPTOR_SERIALIZER_RDFXML_ABBREV
libraptor_la_SOURCES += raptor_serialize_rdfxmla.c
endif
if RAPTOR_SERIALIZER_TURTLE
libraptor_la_SOURCES += raptor_serialize_turtle.c
endif
if RAPTOR_SERIALIZER_RSS_1_0
libraptor_la_SOURCES += raptor_serialize_rss.c
endif
if RAPTOR_SERIALIZER_DOT
libraptor_la_SOURCES += raptor_serialize_dot.c
endif
if RAPTOR_SERIALIZER_JSON
libraptor_la_SOURCES += raptor_serialize_json.c
endif
if RAPTOR_NFC_CHECK
libraptor_la_SOURCES += raptor_nfc_data.c raptor_nfc.c raptor_nfc.h
endif
if STRCASECMP
libraptor_la_SOURCES += strcasecmp.c
endif
if PARSEDATE
libraptor_la_SOURCES += parsedate.c
endif

libraptor_la_LDFLAGS = -version-info @RAPTOR_LIBTOOL_VERSION@
libraptor_la_LIBADD = @LTLIBOBJS@


EXTRA_DIST=\
raptor-config.in \
raptor_www_test.c \
raptor_nfc_test.c \
raptor_win32.c \
$(man_MANS) \
turtle_lexer.l turtle_parser.y \
n3_lexer.l n3_parser.y \
parsedate.y \
fix-flex fix-bison


LEX=@LEX@
YACC=@YACC@

if MAINTAINER_MODE
# Actually it needs turtle_parser.h but nevermind
turtle_lexer.c: $(srcdir)/turtle_lexer.l turtle_parser.c $(srcdir)/fix-flex
	$(LEX) -o$@ $(srcdir)/turtle_lexer.l
	$(PERL) $(srcdir)/fix-flex $@ > turtle_lexer.t
	mv turtle_lexer.t $@

turtle_parser.c: $(srcdir)/turtle_parser.y $(srcdir)/fix-bison
	$(YACC) -b turtle_parser -p turtle_parser_ -d -v $(srcdir)/turtle_parser.y
	$(PERL) $(srcdir)/fix-bison turtle_parser.tab.c > $@
	mv turtle_parser.tab.h turtle_parser.h
	rm -f turtle_parser.tab.c

# Actually it needs n3_parser.h but nevermind
n3_lexer.c: $(srcdir)/n3_lexer.l n3_parser.c $(srcdir)/fix-flex
	$(LEX) -o$@ $(srcdir)/n3_lexer.l
	$(PERL) $(srcdir)/fix-flex $@ > n3_lexer.t
	mv n3_lexer.t $@

n3_parser.c: $(srcdir)/n3_parser.y  $(srcdir)/fix-bison
	$(YACC) -b n3_parser -p n3_parser_ -d -v $(srcdir)/n3_parser.y
	$(PERL) $(srcdir)/fix-bison n3_parser.tab.c > $@
	mv n3_parser.tab.h n3_parser.h
	rm -f n3_parser.tab.c

endif

# Actually it needs turtle_parser.h but nevermind
turtle_lexer_test: $(srcdir)/turtle_lexer.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/turtle_lexer.c libraptor.la $(LIBS)

turtle_parser_test: $(srcdir)/turtle_parser.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/turtle_parser.c libraptor.la $(LIBS)


if MAINTAINER_MODE
n3_lexer_test: $(srcdir)/n3_lexer.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/n3_lexer.c libraptor.la $(LIBS)

n3_parser_test: $(srcdir)/n3_parser.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/n3_parser.c libraptor.la $(LIBS)
endif


parsedate.c: $(srcdir)/parsedate.y
	$(YACC) -b parsedate -p raptor_parsedate_ -d -v $(srcdir)/parsedate.y
	sed -e '/Suppress GCC warning that yyerrlab1/,/^\#endif/d' -e "s/parsedate.tab.c/$@/" parsedate.tab.c > $@
	mv parsedate.tab.h parsedate.h
	rm -f parsedate.tab.c



# Some people need a little help ;-)
test: check

raptor_parse_test: $(srcdir)/raptor_general.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/raptor_parse.c libraptor.la $(LIBS)

raptor_rfc2396_test: $(srcdir)/raptor_rfc2396.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/raptor_rfc2396.c libraptor.la $(LIBS)

raptor_uri_test: $(srcdir)/raptor_uri.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/raptor_uri.c libraptor.la $(LIBS)

raptor_uri_win32_test: $(srcdir)/raptor_uri.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE -DWIN32_URI_TEST $(srcdir)/raptor_uri.c libraptor.la $(LIBS)

raptor_namespace_test: $(srcdir)/raptor_namespace.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/raptor_namespace.c libraptor.la $(LIBS)

strcasecmp_test: $(srcdir)/strcasecmp.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/strcasecmp.c libraptor.la $(LIBS)

raptor_www_test: $(srcdir)/raptor_www_test.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/raptor_www_test.c libraptor.la $(LIBS)

raptor_set_test: $(srcdir)/raptor_set.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/raptor_set.c libraptor.la $(LIBS)

raptor_xml_test: $(srcdir)/raptor_xml.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/raptor_xml.c libraptor.la $(LIBS)

raptor_sequence_test: $(srcdir)/raptor_sequence.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/raptor_sequence.c libraptor.la $(LIBS)

raptor_stringbuffer_test: $(srcdir)/raptor_stringbuffer.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/raptor_stringbuffer.c libraptor.la $(LIBS)

raptor_nfc_test: $(srcdir)/raptor_nfc_test.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/raptor_nfc_test.c libraptor.la $(LIBS)

raptor_iostream_test: $(srcdir)/raptor_iostream.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/raptor_iostream.c libraptor.la $(LIBS)

raptor_xml_writer_test: $(srcdir)/raptor_xml_writer.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/raptor_xml_writer.c libraptor.la $(LIBS)

raptor_turtle_writer_test: $(srcdir)/raptor_turtle_writer.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/raptor_turtle_writer.c libraptor.la $(LIBS)

raptor_avltree_test: $(srcdir)/raptor_avltree.c libraptor.la
	$(LINK) $(DEFS) $(CPPFLAGS) -I$(srcdir) -I. -DSTANDALONE $(srcdir)/raptor_avltree.c libraptor.la $(LIBS)
