# -*- Mode: Makefile -*-
#
# Makefile.am - top level automake file for Redland
#
# Copyright (C) 2000-2006, David Beckett http://purl.org/net/dajobe/
# Copyright (C) 2000-2004, Copyright (C) 2000-2006, University of Bristol, UK http://www.bristol.ac.uk/
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

SUBDIRS=win32

lib_LTLIBRARIES = librdf.la

include_HEADERS = redland.h librdf.h \
rdf_init.h \
rdf_uri.h rdf_node.h rdf_statement.h rdf_concepts.h \
rdf_model.h \
rdf_iterator.h \
rdf_storage.h \
rdf_stream.h \
rdf_parser.h \
rdf_utf8.h \
rdf_query.h \
rdf_serializer.h \
rdf_log.h \
rdf_digest.h \
rdf_hash.h

noinst_HEADERS = win32_rdf_config.h

librdf_la_SOURCES = rdf_init.c \
rdf_uri.c rdf_node.c rdf_statement.c \
rdf_digest.c rdf_hash.c rdf_hash_cursor.c rdf_hash_memory.c \
rdf_model.c rdf_model_storage.c \
rdf_iterator.c rdf_concepts.c \
rdf_avltree.c \
rdf_list.c \
rdf_storage.c rdf_storage_list.c rdf_storage_hashes.c rdf_storage_trees.c \
rdf_storage_sql.c \
rdf_stream.c \
rdf_parser.c rdf_parser_raptor.c \
rdf_heuristics.c rdf_files.c rdf_utf8.c \
rdf_query.c rdf_query_results.c \
rdf_query_triples.c rdf_query_rasqal.c \
rdf_serializer.c \
rdf_serializer_raptor.c \
rdf_log.c \
redland.h librdf.h \
rdf_internal.h \
rdf_init.h \
rdf_uri.h rdf_node.h rdf_statement.h rdf_concepts.h \
rdf_digest.h rdf_hash.h \
rdf_types.h \
rdf_model.h \
rdf_iterator.h \
rdf_list.h \
rdf_storage.h \
rdf_stream.h \
rdf_parser.h \
rdf_heuristics.h rdf_files.h rdf_utf8.h \
rdf_query.h \
rdf_serializer.h \
rdf_log.h \
rdf_avltree_internal.h \
rdf_concepts_internal.h \
rdf_digest_internal.h \
rdf_hash_internal.h \
rdf_init_internal.h \
rdf_iterator_internal.h \
rdf_list_internal.h \
rdf_log_internal.h \
rdf_model_internal.h \
rdf_node_internal.h \
rdf_parser_internal.h \
rdf_query_internal.h \
rdf_serializer_internal.h \
rdf_statement_internal.h \
rdf_storage_internal.h \
rdf_stream_internal.h \
rdf_uri_internal.h


if MEMCMP
librdf_la_SOURCES += memcmp.c
endif

plugindir = $(libdir)/redland

if MODULAR_LIBRDF

plugin_LTLIBRARIES =

if STORAGE_TSTORE
plugin_LTLIBRARIES += librdf_storage_tstore.la
librdf_storage_tstore_la_SOURCES = rdf_storage_tstore.c
librdf_storage_tstore_la_CPPFLAGS = @TSTORE_CPPFLAGS@
librdf_storage_tstore_la_LIBADD = @TSTORE_LIBS@
librdf_storage_tstore_la_LDFLAGS = -module -avoid-version
endif

if STORAGE_MYSQL
plugin_LTLIBRARIES += librdf_storage_mysql.la
librdf_storage_mysql_la_SOURCES = rdf_storage_mysql.c
librdf_storage_mysql_la_CPPFLAGS = @MYSQL_CPPFLAGS@
librdf_storage_mysql_la_LIBADD = @MYSQL_LIBS@
librdf_storage_mysql_la_LDFLAGS = -module -avoid-version
endif

if STORAGE_SQLITE
plugin_LTLIBRARIES += librdf_storage_sqlite.la
librdf_storage_sqlite_la_SOURCES = rdf_storage_sqlite.c
librdf_storage_sqlite_la_CPPFLAGS = @SQLITE_CPPFLAGS@
librdf_storage_sqlite_la_LIBADD = @SQLITE_LIBS@
librdf_storage_sqlite_la_LDFLAGS = -module -avoid-version
endif

if STORAGE_POSTGRESQL
plugin_LTLIBRARIES += librdf_storage_postgresql.la
librdf_storage_postgresql_la_SOURCES = rdf_storage_postgresql.c
librdf_storage_postgresql_la_CPPFLAGS = @POSTGRESQL_CPPFLAGS@
librdf_storage_postgresql_la_LIBADD = @POSTGRESQL_LIBS@
librdf_storage_postgresql_la_LDFLAGS = -module -avoid-version
endif

else

if STORAGE_FILE
librdf_la_SOURCES += rdf_storage_file.c
endif

if STORAGE_TSTORE
librdf_la_SOURCES += rdf_storage_tstore.c
endif

if STORAGE_MYSQL
librdf_la_SOURCES += rdf_storage_mysql.c
endif

if STORAGE_SQLITE
librdf_la_SOURCES += rdf_storage_sqlite.c
endif

if STORAGE_POSTGRESQL
librdf_la_SOURCES += rdf_storage_postgresql.c
endif

endif

librdf_la_LIBADD = \
@LTLIBOBJS@ @DIGEST_OBJS@ @HASH_OBJS@ \
@LIBRDF_INTERNAL_LIBS@

librdf_la_DEPENDENCIES = \
@DIGEST_OBJS@ @HASH_OBJS@ \
@LIBRDF_INTERNAL_LIBS@

EXTRA_librdf_la_SOURCES = rdf_hash_bdb.c \
rdf_digest_md5.c rdf_digest_sha1.c rdf_digest_openssl.c \
rdf_parser_raptor.c

EXTRA_DIST=\
redland.spec redland.spec.in

librdf_la_LDFLAGS = -version-info @LIBRDF_LIBTOOL_VERSION@ \
@LIBRDF_LDFLAGS@ @LIBRDF_EXTERNAL_LIBS@ $(MEM_LIBS)

pkgdata_DATA=
if STORAGE_MYSQL
pkgdata_DATA += mysql-v1.ttl mysql-v2.ttl
endif

EXTRA_DIST += mysql-v1.ttl mysql-v2.ttl

local_tests=rdf_storage_sql_test$(EXEEXT)

EXTRA_PROGRAMS=$(local_tests)

TESTS=rdf_node_test rdf_digest_test rdf_hash_test rdf_uri_test \
rdf_statement_test rdf_model_test rdf_storage_test rdf_parser_test \
rdf_files_test rdf_heuristics_test rdf_utf8_test rdf_concepts_test \
rdf_query_test rdf_serializer_test rdf_stream_test rdf_iterator_test \
rdf_init_test

CLEANFILES=$(TESTS) $(local_tests) test test*.db test.rdf

# Memory debugging alternatives
MEM=@MEM@
MEM_LIBS=@MEM_LIBS@

# 1) None (use standard functions directly)
#MEM=
#MEM_LIBS=

# 2) Use internal memory debug
#MEM=-DLIBRDF_MEMORY_DEBUG=1
#MEM_LIBS=

# 3) Use dmalloc library
#MEM=-DLIBRDF_MEMORY_DEBUG_DMALLOC=1
#MEM_LIBS=-ldmalloc


# Use tar, whatever it is called (better be GNU tar though)
TAR=@TAR@

# Why is this not in the default makefile?
CC=@CC@

AM_CPPFLAGS=@LIBRDF_INTERNAL_CPPFLAGS@ @LIBRDF_CPPFLAGS@ @LIBRDF_EXTERNAL_CPPFLAGS@
AM_CFLAGS=@LIBRDF_INTERNAL_CPPFLAGS@ @LIBRDF_CPPFLAGS@ @LIBRDF_EXTERNAL_CPPFLAGS@ $(MEM)
STANDARD_CFLAGS=@STANDARD_CFLAGS@ $(MEM)

AM_CPPFLAGS += -DPKGDATADIR="\"$(pkgdatadir)\""
if MODULAR_LIBRDF
AM_CFLAGS += -DLIBRDF_MODULE_DIR="\"$(libdir)/redland\""
endif

LOCAL_LIB_DIR=lib

# Rules to construct test programs

check-local: $(local_tests) run-local-tests

rdf_storage_sql_test_SOURCES = rdf_storage_sql_test.c
rdf_storage_sql_test_LDADD = librdf.la


run-local-tests: rdf_storage_sql_test$(EXEEXT)
	@tests="rdf_storage_sql_test"; \
	status=0; \
	for tst in $$tests; do \
	  if test -f ./$$tst; then dir=./; \
	  elif test -f $$tst; then dir=; \
	  else dir="$(top_srcdir)/"; fi; \
	  if $${dir}$$tst $(srcdir); then \
	    echo "PASS: $$tst"; \
	  else \
	    echo "FAIL: $$tst"; \
	    status=1; \
	    break; \
	  fi; \
	done; \
	exit $$status


# Some people need a little help ;-)
test: check

# rule for building tests in one step
COMPILE_LINK = $(LIBTOOL) --tag=CC --mode=link $(CCLD) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@

rdf_node_test: rdf_node.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_node.c librdf.la

rdf_digest_test: rdf_digest.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_digest.c librdf.la

rdf_hash_test: rdf_hash.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_hash.c librdf.la

rdf_uri_test: rdf_uri.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_uri.c librdf.la

rdf_statement_test: rdf_statement.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_statement.c librdf.la

rdf_model_test: rdf_model.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_model.c librdf.la

rdf_storage_test: rdf_storage.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_storage.c librdf.la

rdf_parser_test: rdf_parser.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_parser.c librdf.la

rdf_files_test: rdf_files.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_files.c librdf.la

rdf_heuristics_test: rdf_heuristics.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_heuristics.c librdf.la

rdf_utf8_test: rdf_utf8.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_utf8.c librdf.la

rdf_concepts_test: rdf_concepts.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_concepts.c librdf.la

rdf_query_test: rdf_query.c rdf_query_results.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_query.c $(srcdir)/rdf_query_results.c librdf.la

rdf_serializer_test: rdf_serializer.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_serializer.c librdf.la

rdf_stream_test: rdf_stream.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_stream.c librdf.la

rdf_iterator_test: rdf_iterator.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_iterator.c librdf.la

rdf_avltree_test: rdf_avltree.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_avltree.c librdf.la

rdf_init_test: rdf_init.c librdf.la
	$(COMPILE_LINK) -DSTANDALONE $(srcdir)/rdf_init.c librdf.la

@SET_MAKE@

$(top_builddir)/raptor/src/libraptor.la:
	cd $(top_builddir)/raptor/src && $(MAKE) $(AM_MAKEFLAGS) libraptor.la

$(top_builddir)/rasqal/src/librasqal.la:
	cd $(top_builddir)/rasqal/src && $(MAKE) $(AM_MAKEFLAGS) librasqal.la
