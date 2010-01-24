
## Man Page
man_MANS = twolame.1


## The documentation is distributed with the package
## and only needs to be rebuilt if it is modified or
## when building from subversion

doc: build-man build-html


## Building the manpage requires:
##    asciidoc:	http://www.methods.co.nz/asciidoc/
##    xmlto:	http://cyberelk.net/tim/xmlto/
##

build-man:
	rm -f twolame.1.xml twolame.1
	asciidoc -d manpage -b docbook -o twolame.1.xml twolame.1.txt
	xmlto man twolame.1.xml



## Building HTML documentation requires:
##    asciidoc:	http://www.methods.co.nz/asciidoc/
##    doxygen:	http://www.doxygen.org
##

htmldir=$(srcdir)/html
asciidoc=asciidoc -b xhtml11 \
	-a revision="@PACKAGE_VERSION@" \
	-a theme=twolame \
	-a linkcss \
	-a stylesdir=. 
doxygen=doxygen doxygen.conf


build-html:
	rm -f $(htmldir)/*.html
	$(doxygen)
	rm -f $(htmldir)/index.html $(htmldir)/files.html $(htmldir)/globals*.html
	$(asciidoc) -o $(htmldir)/index.html $(srcdir)/index.txt
	$(asciidoc) -o $(htmldir)/readme.html $(top_srcdir)/README
	$(asciidoc) -o $(htmldir)/authors.html $(top_srcdir)/AUTHORS
	$(asciidoc) -o $(htmldir)/changelog.html $(top_srcdir)/ChangeLog
	$(asciidoc) -o $(htmldir)/todo.html $(top_srcdir)/TODO
	$(asciidoc) -o $(htmldir)/api.html $(srcdir)/api.txt
	$(asciidoc) -o $(htmldir)/psycho.html $(srcdir)/psycho.txt
	$(asciidoc) -o $(htmldir)/vbr.html $(srcdir)/vbr.txt
	$(asciidoc) -d manpage -o $(htmldir)/twolame.1.html $(srcdir)/twolame.1.txt


## Install HTML Documention
docdir = $(datadir)/doc
pkgdocdir = $(docdir)/$(PACKAGE)
pkghtmldir = $(pkgdocdir)/html

pkghtml_DATA = \
    $(htmldir)/api.html \
    $(htmldir)/authors.html \
    $(htmldir)/doxygen.css \
    $(htmldir)/doxygen.png \
    $(htmldir)/index.html \
    $(htmldir)/changelog.html \
    $(htmldir)/psycho.html \
    $(htmldir)/readme.html \
    $(htmldir)/todo.html \
    $(htmldir)/twolame_8h-source.html \
    $(htmldir)/twolame_8h.html \
    $(htmldir)/twolame.1.html \
    $(htmldir)/twolame.css \
    $(htmldir)/twolame-manpage.css \
    $(htmldir)/twolame-quirks.css \
    $(htmldir)/vbr.html

pkgdoc_DATA = \
	$(srcdir)/api.txt \
	$(srcdir)/psycho.txt \
	$(srcdir)/vbr.txt

EXTRA_DIST = \
	$(man_MANS) \
	$(pkgdoc_DATA) \
	$(pkghtml_DATA) \
	dist10-text/*.txt \
	doxygen.conf \
	index.txt \
	twolame.1.txt \
	twolame.1.xml

