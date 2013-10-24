## Process this file with automake to produce Makefile.in


#AUTOMAKE_OPTIONS = foreign 1.6 dist-zip
AUTOMAKE_OPTIONS = foreign 1.11 dist-zip dist-xz

SUBDIRS = src include doc

m4datadir = $(datadir)/aclocal
m4data_DATA = ogg.m4

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = ogg.pc

EXTRA_DIST = README AUTHORS CHANGES COPYING \
	libogg.spec libogg.spec.in \
	ogg.m4 ogg.pc.in ogg-uninstalled.pc.in \
	macosx win32

dist-hook:
	for item in $(EXTRA_DIST); do \
	  if test -d $$item; then \
	    echo -n "cleaning dir $$item for distribution..."; \
	    rm -rf `find $(distdir)/$$item -name .svn`; \
	    echo "OK"; \
	  fi; \
	done
debug:
	$(MAKE) all CFLAGS="@DEBUG@"

profile:
	$(MAKE) all CFLAGS="@PROFILE@"
