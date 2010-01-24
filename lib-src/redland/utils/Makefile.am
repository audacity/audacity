MYSQL_UTILS=rdf-tree

bin_PROGRAMS=redland-db-upgrade rdfproc
noinst_SCRIPTS=touch-mtime.pl

AM_INSTALLCHECK_STD_OPTIONS_EXEMPT=redland-db-upgrade 

EXTRA_PROGRAMS=$(MYSQL_UTILS)

man_MANS = redland-db-upgrade.1 rdfproc.1

EXTRA_DIST= rdfproc.html \
$(man_MANS) \
touch-mtime.pl

CLEANFILES=*.db $(EXTRA_PROGRAMS)

AM_CPPFLAGS=@LIBRDF_INTERNAL_CPPFLAGS@ @LIBRDF_CPPFLAGS@ -I$(top_srcdir)/librdf @LIBRDF_EXTERNAL_CPPFLAGS@
AM_CFLAGS=@CFLAGS@ $(MEM)

redland_db_upgrade_SOURCES = db_upgrade.c
redland_db_upgrade_LDADD=@LIBRDF_LDFLAGS@ $(top_builddir)/librdf/librdf.la

rdfproc_SOURCES = rdfproc.c
if GETOPT
rdfproc_SOURCES += getopt.c rdfproc_getopt.h
endif
rdfproc_LDADD=@LIBRDF_LDFLAGS@ $(top_builddir)/librdf/librdf.la

mysql-utils: $(MYSQL_UTILS)

@MAINT@rdfproc.html: $(srcdir)/rdfproc.1 $(top_srcdir)/raptor/fix-groff-xhtml
@MAINT@	-groff -man -Thtml -P-l $< | tidy -asxml -wrap 1000 2>/dev/null | $(PERL) $(top_srcdir)/raptor/fix-groff-xhtml $@

