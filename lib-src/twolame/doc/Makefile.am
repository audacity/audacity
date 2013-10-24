SUBDIRS=html


## Building the manpage requires:
##    asciidoc:	http://www.methods.co.nz/asciidoc/
##    xmlto:	http://cyberelk.net/tim/xmlto/
##

## Man Page
man_MANS = twolame.1

if MAINTAINER_MODE

twolame.1.xml: twolame.1.txt
	asciidoc -d manpage -b docbook -o twolame.1.xml twolame.1.txt

twolame.1: twolame.1.xml
	@PATH_XMLTO@ man twolame.1.xml

endif

MAINTAINERCLEANFILES=twolame.1 twolame.1.xml

pkgdocdir = @docdir@
pkgdoc_DATA = \
	api.txt \
	psycho.txt \
	vbr.txt

EXTRA_DIST = \
	$(man_MANS) \
	$(pkgdoc_DATA) \
	dist10-text/common.txt \
	dist10-text/commonh.txt \
	dist10-text/encode.txt \
	dist10-text/encoderh.txt \
	dist10-text/musicin.txt \
	dist10-text/psy.txt \
	dist10-text/tonal.txt \
	index.txt \
	twolame.1.txt
