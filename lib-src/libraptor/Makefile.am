# -*- Mode: Makefile -*-
#
# Makefile.am - top level automake file for Raptor
#
# Copyright (C) 2000-2006 David Beckett http://purl.org/net/dajobe/
# Copyright (C) 2000-2005 University of Bristol, UK http://www.bristol.ac.uk/
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


noinst_SCRIPTS = raptor-src-config

SUBDIRS= src utils docs data tests win32 examples

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = raptor.pc


EXTRA_DIST=\
ChangeLog ChangeLog.1 ChangeLog.2 ChangeLog.3 ChangeLog.4 ChangeLog.5 \
ChangeLog.6 ChangeLog.7 \
README      NEWS      LICENSE.txt \
README.html NEWS.html LICENSE.html INSTALL.html \
RELEASE.html \
LICENSE-2.0.txt NOTICE \
raptor.rdf.in \
raptor-src-config.in \
autogen.sh \
raptor.spec.in \
fix-groff-xhtml \
raptor.pc.in

if RELEASE_VERSION
EXTRA_DIST += raptor.spec
endif

DISTCHECK_CONFIGURE_FLAGS=--enable-gtk-doc

# Create some text files from HTML sources
LYNX=lynx
HTML_TO_TEXT=TERM=vt100 $(LYNX) -dump -nolist

SUFFIXES = .html .txt

.html.txt:
	$(HTML_TO_TEXT) $< > $@

README: README.html
	$(HTML_TO_TEXT) $< > $@

NEWS: NEWS.html
	$(HTML_TO_TEXT) $< > $@

# Some people need a little help ;-)
test: check

dist-hook: README NEWS
	@for file in README NEWS; do \
          if test -r $(srcdir)/$$file; then \
	    rm -f $(distdir)/$$file; \
	    cp -p $(srcdir)/$$file $(distdir)/$$file; \
	  fi; \
	done
