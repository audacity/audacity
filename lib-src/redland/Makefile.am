# -*- Mode: Makefile -*-
#
# Makefile.am - top level automake file for Redland
#
# $Id: Makefile.am,v 1.1 2008-07-08 10:39:42 larsl Exp $
#
# Copyright (C) 2000-2007 David Beckett http://purl.org/net/dajobe/
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

bin_SCRIPTS = redland-config
noinst_SCRIPTS = redland-src-config

man_MANS = redland-config.1

dist_pkgdata_DATA = Redland.i

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = redland.pc

DISTCHECK_CONFIGURE_FLAGS=--enable-gtk-doc

# Subdirectories to build/install/distribute etc.
#
# NOTE: A full distribution tarball cannot be made inside a 'make distcheck'
# when raptor and rasqal are installed system wide, since then the
# subdirs are not configured or included in the build tree, when it
# is separate from the source tree.
#
# always make the release tarball from the top level and
# use configure --with-raptor=internal and --with-rasqal=internal
# OR --enable-maintainer-mode
# to ensure that the raptor and rasqal subdirectories are called
# as CONFIG_SUBDIR directories.
#
SUBDIRS=@LOCAL_SUBDIRS@ librdf examples utils demos docs data

EXTRA_DIST=ChangeLog ChangeLog.1 ChangeLog.2 ChangeLog.3 ChangeLog.4 \
ChangeLog.5 ChangeLog.6 ChangeLog.7 \
README      NEWS      LICENSE.txt  TODO \
README.html NEWS.html LICENSE.html TODO.html \
RELEASE.html INSTALL.html FAQS.html \
LICENSE-2.0.txt NOTICE \
redland.rdf.in \
autogen.sh \
redland-config.in \
redland-src-config.in \
$(man_MANS) \
redland.sln \
gtk-doc.make \
redland.pc.in

if MAINTAINER_MODE
# Create some text files from HTML sources
LYNX=lynx

HTML_TO_TEXT=TERM=vt100 $(LYNX) -dump $< | sed -e 's,file://localhost.*librdf/,,' > $@

SUFFIXES = .html .txt

.html.txt:
	$(HTML_TO_TEXT)

README: README.html
	$(HTML_TO_TEXT)

NEWS: NEWS.html
	$(HTML_TO_TEXT)

TODO: TODO.html
	$(HTML_TO_TEXT)
endif

@SET_MAKE@

install-data-hook:
	$(PERL) $(srcdir)/utils/touch-mtime.pl $(srcdir)/Redland.i $(DESTDIR)$(pkgdatadir)/Redland.i
