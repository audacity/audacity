# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for Rasqal RDF query language library
#
# Copyright (C) 2003-2008, David Beckett http://www.dajobe.org/
# Copyright (C) 2003-2005, University of Bristol, UK http://www.bristol.ac.uk/
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

noinst_SCRIPTS = rasqal-src-config

SUBDIRS=src utils tests docs data win32

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = rasqal.pc


EXTRA_DIST=ChangeLog ChangeLog.1 ChangeLog.2 ChangeLog.3 ChangeLog.4 \
README      NEWS      LICENSE.txt \
README.html NEWS.html LICENSE.html INSTALL.html \
RELEASE.html \
LICENSE-2.0.txt NOTICE \
rasqal.rdf.in \
autogen.sh \
rasqal-src-config.in \
rasqal.spec.in \
fix-groff-xhtml \
$(man_MANS) \
rasqal.pc.in

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
