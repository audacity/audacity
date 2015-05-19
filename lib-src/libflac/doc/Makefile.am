#  flac - Command-line FLAC encoder/decoder
#  Copyright (C) 2002-2009  Josh Coalson
#  Copyright (C) 2011-2014  Xiph.Org Foundation
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License along
#  with this program; if not, write to the Free Software Foundation, Inc.,
#  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

SUBDIRS = . html

if FLaC__HAS_DOXYGEN
all-local: Doxyfile
FLAC.tag: Doxyfile
	doxygen Doxyfile
	rm -rf html/api
	mv doxytmp/html html/api
	rm -rf doxytmp
else
FLAC.tag:
	touch $@
	mkdir -p html/api
endif

docdir = $(datadir)/doc/$(PACKAGE)-$(VERSION)

doc_DATA = \
	FLAC.tag

EXTRA_DIST = Doxyfile.in Makefile.lite doxygen.footer.html doxygen.header.html $(doc_DATA)

distclean-local:
	rm -rf FLAC.tag html/api doxytmp
