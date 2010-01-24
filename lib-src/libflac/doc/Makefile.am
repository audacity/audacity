#  flac - Command-line FLAC encoder/decoder
#  Copyright (C) 2002  Josh Coalson
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
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

AUTOMAKE_OPTIONS = foreign

SUBDIRS = . html

FLAC.tag: Doxyfile
	doxygen Doxyfile
	rm -rf html/api
	mv doxytmp/html html/api
	rm -rf doxytmp

docdir = $(datadir)/doc/$(PACKAGE)-$(VERSION)

doc_DATA = \
	FLAC.tag

EXTRA_DIST = Doxyfile Makefile.lite $(doc_DATA)

clean-local:
	rm -rf FLAC.tag html/api doxytmp
