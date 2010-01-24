# Copyright (C) 1999 Scott Thomas Haug <scott@id3.org>
#  
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without 
# modifications, as long as this notice is preserved.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# require automake 1.4
AUTOMAKE_OPTIONS = 1.4

EXTRA_DIST =                    \
        HISTORY                 \
        config.h.win32          \
        config.h.win32.in       \
        id3lib.spec             \
        id3lib.spec.in          \
        reconf
if ID3_NEEDZLIB
zlib_subdir = zlib
else
zlib_subdir = 
endif

SUBDIRS =  . m4 $(zlib_subdir) doc include src examples
DIST_SUBDIRS = . m4 zlib doc include src examples

INCLUDES = @ID3LIB_DEBUG_FLAGS@

config.h.win32: $(top_builddir)/config.status $(top_srcdir)/config.h.win32.in 
	cd $(top_builddir) && CONFIG_FILES=$@ CONFIG_HEADERS= $(SHELL) ./config.status

id3lib.spec: $(top_builddir)/config.status $(top_srcdir)/id3lib.spec.in 
	cd $(top_builddir) && CONFIG_FILES=$@ CONFIG_HEADERS= $(SHELL) ./config.status

docsdistdir = $(PACKAGE)-doc-$(VERSION)

.PHONY: release snapshot docs-release docs

changelog:
	./cvs2cl.pl --tags --branches --revisions --day-of-week --prune --fsf -U AUTHORS -W 3600

docs:
	-cd doc && $(MAKE) $(AM_MAKEFLAGS) $@

docs-release: docs
	-mv doc/$(docsdistdir).* .
	-cd examples && $(MAKE) $(AM_MAKEFLAGS) clean
	-mkdir $(docsdistdir)
	-cp -R examples           $(docsdistdir)
	-cp    doc/*.*            $(docsdistdir)
	-cp -R doc/@DOX_DIR_HTML@ $(docsdistdir)
	-cp    NEWS               $(docsdistdir)/NEWS.txt
	-cp    ChangeLog          $(docsdistdir)/ChangeLog.txt
	-find $(docsdistdir) -name "Makefile*" -exec rm -f {} \;
	-find $(docsdistdir) -name "*.mp3"     -exec rm -f {} \;
	GZIP=$(GZIP_ENV) $(TAR) zcf $(docsdistdir).tar.gz $(docsdistdir)
	-rm -rf $(docsdistdir)

release: config.h.win32 id3lib.spec
	-rm -rf .deps */.deps $(distdir).zip
	$(MAKE) $(AM_MAKEFLAGS) distcheck
	GZIP=$(GZIP_ENV) $(TAR) zxf $(distdir).tar.gz
	-cp -R id3com/ prj/ libprj/ delphi/ $(distdir)
	-find $(distdir) -name "*~" -exec rm {} \;
	-find $(distdir) -type d -name CVS -exec rm -r {} \;
	-find $(distdir) -name .cvsignore -exec rm {} \;
	-find $(distdir) \( -name "*.dsp" -or -name "*.dsw" \) -exec unix2dos {} \;
	cd $(distdir) && cp config.h.win32 config.h
	cd $(distdir) && zip -r ../$(distdir).zip *
	cd $(distdir) && ./configure && $(MAKE) $(AM_MAKEFLAGS) docs-release
	mv $(distdir)/$(docsdistdir).* .
	-rm -rf $(distdir)

snapshot: config.h.win32
	ss_distdir=$(PACKAGE)-`date +"%Y%m%d"`; \
	$(MAKE) $(AM_MAKEFLAGS) distdir distdir=$$ss_distdir; \
	chmod -R a+r $$ss_distdir; \
	GZIP=$(GZIP_ENV) $(TAR) chozf $${ss_distdir}.tar.gz $$ss_distdir; \
	cd $$ss_distdir && cp config.h.win32 config.h && cd ..; \
	cd $$ss_distdir && zip -r ../$${ss_distdir}.zip * && cd ..; \
	rm -rf $$ss_distdir
