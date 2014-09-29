
asciidoc=@PATH_ASCIIDOC@ -b xhtml11 \
	-a revision="@PACKAGE_VERSION@" \
	-a theme=twolame \
	-a linkcss \
	-a stylesdir=$(srcdir)

pkgdocdir = @docdir@
pkghtmldir = $(pkgdocdir)/html
    
doxygen_files = \
    doxygen.css \
    doxygen.png \
    nav_f.png \
    nav_h.png \
    tabs.css \
    twolame_8h_source.html \
    twolame_8h.html

asciidoc_files = \
    api.html \
    authors.html \
    changelog.html \
    index.html \
    psycho.html \
    readme.html \
    todo.html \
    twolame.1.html \
    vbr.html

stylesheet_files = \
    twolame-manpage.css \
    twolame.css
    

pkghtml_DATA = $(doxygen_files) $(asciidoc_files) $(stylesheet_files)
EXTRA_DIST = Doxyfile.in $(pkghtml_DATA)


if MAINTAINER_MODE

$(doxygen_files): Doxyfile $(top_srcdir)/libtwolame/twolame.h
	@PATH_DOXYGEN@
	rm -f index.html files.html globals*.html

index.html: $(top_srcdir)/doc/index.txt
	$(asciidoc) -o $@ $<

readme.html: $(top_srcdir)/README
	$(asciidoc) -o $@ $<

authors.html: $(top_srcdir)/AUTHORS
	$(asciidoc) -o $@ $<

changelog.html: $(top_srcdir)/ChangeLog
	$(asciidoc) -o $@ $<

todo.html: $(top_srcdir)/TODO
	$(asciidoc) -o $@ $<

api.html: $(top_srcdir)/doc/api.txt
	$(asciidoc) -o $@ $<

psycho.html: $(top_srcdir)/doc/psycho.txt
	$(asciidoc) -o $@ $<

vbr.html: $(top_srcdir)/doc/vbr.txt
	$(asciidoc) -o $@ $<

twolame.1.html: $(top_srcdir)/doc/twolame.1.txt
	$(asciidoc) -d manpage -o $@ $<

endif

MAINTAINERCLEANFILES = $(doxygen_files) $(asciidoc_files)
