SUBDIRS = taglib bindings tests

AUTOMAKE_OPTIONS = foreign

$(top_srcdir)/configure.in: configure.in.in $(top_srcdir)/subdirs
	cd $(top_srcdir) && $(MAKE) -f admin/Makefile.common configure.in ;

$(top_srcdir)/subdirs:
	cd $(top_srcdir) && $(MAKE) -f admin/Makefile.common subdirs

$(top_srcdir)/acinclude.m4: $(top_srcdir)/admin/acinclude.m4.in $(top_srcdir)/admin/libtool.m4.in
	@cd $(top_srcdir) && cat admin/acinclude.m4.in admin/libtool.m4.in > acinclude.m4

MAINTAINERCLEANFILES = subdirs configure.in acinclude.m4 configure.files

bin_SCRIPTS = taglib-config

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = taglib.pc

package-messages:
	$(MAKE) -f admin/Makefile.common package-messages

dist-hook:
	cd $(top_distdir) && perl admin/am_edit -padmin
	cd $(top_distdir) && $(MAKE) -f admin/Makefile.common subdirs

examples: examples-all

examples-all:
	cd examples ; \
	$(MAKE) all

apidox:
	$(mkinstalldirs) doc/api; \
	if test ! -x doc/common; then \
		$(LN_S) $(kde_libs_htmldir)/en/common doc/common ; \
	fi; \
	cp $(top_srcdir)/admin/Doxyfile.global taglib.doxyfile; \
	echo "PROJECT_NAME        = TagLib" >> taglib.doxyfile; \
	echo "PROJECT_NUMBER      = \"Version 1.5\"" >> taglib.doxyfile; \
	echo "INPUT               = $(srcdir)" >> taglib.doxyfile; \
	echo "OUTPUT_DIRECTORY    = doc/api" >> taglib.doxyfile; \
	echo "HTML_OUTPUT         = html" >> taglib.doxyfile; \
	echo "GENERATE_HTML       = YES" >> taglib.doxyfile ; \
	echo "GENERATE_MAN        = NO" >> taglib.doxyfile ; \
	echo "GENERATE_LATEX      = NO" >> taglib.doxyfile ; \
	echo "HTML_HEADER         = doc/common/header.html" >> taglib.doxyfile ; \
	echo "HTML_FOOTER         = doc/common/footer.html" >> taglib.doxyfile ; \
	echo "HTML_STYLESHEET     = doc/common/doxygen.css" >> taglib.doxyfile ; \
	echo "FILE_PATTERNS       = *.h" >> taglib.doxyfile ; \
	echo "PREDEFINED          = DO_NOT_DOCUMENT DOXYGEN" >> taglib.doxyfile ; \
	echo "EXTRACT_ALL         = YES" >> taglib.doxyfile ; \
	doxygen taglib.doxyfile
