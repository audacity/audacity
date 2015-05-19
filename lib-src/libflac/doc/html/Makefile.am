#  FLAC - Free Lossless Audio Codec
#  Copyright (C) 2001-2009  Josh Coalson
#  Copyright (C) 2011-2014  Xiph.Org Foundation
#
#  This file is part the FLAC project.  FLAC is comprised of several
#  components distributed under different licenses.  The codec libraries
#  are distributed under Xiph.Org's BSD-like license (see the file
#  COPYING.Xiph in this distribution).  All other programs, libraries, and
#  plugins are distributed under the GPL (see COPYING.GPL).  The documentation
#  is distributed under the Gnu FDL (see COPYING.FDL).  Each file in the
#  FLAC distribution contains at the top the terms under which it may be
#  distributed.
#
#  Since this particular file is relevant to all components of FLAC,
#  it may be distributed under the Xiph.Org license, which is the least
#  restrictive of those mentioned above.  See the file COPYING.Xiph in this
#  distribution.

SUBDIRS = images

docdir = $(datadir)/doc/$(PACKAGE)-$(VERSION)/html

doc_DATA = \
	changelog.html \
	developers.html \
	documentation.html \
	documentation_bugs.html \
	documentation_example_code.html \
	documentation_format_overview.html \
	documentation_tools.html \
	documentation_tools_flac.html \
	documentation_tools_metaflac.html \
	faq.html \
	favicon.ico \
	features.html \
	flac.css \
	format.html \
	id.html \
	index.html \
	license.html \
	ogg_mapping.html

EXTRA_DIST = $(doc_DATA) api

if FLaC__HAS_DOXYGEN
# The install targets don't copy whole directories so we have to
# handle 'api/' specially:
install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(docdir)/api
	(cd $(builddir)/api && $(INSTALL_DATA) * $(DESTDIR)$(docdir)/api)
uninstall-local:
	rm -rf $(DESTDIR)$(docdir)/api
distclean-local:
	-rm -rf api
endif
