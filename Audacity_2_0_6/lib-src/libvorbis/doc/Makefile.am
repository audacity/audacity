## Process this with automake to create Makefile.in

SUBDIRS = libvorbis vorbisfile vorbisenc

docdir = $(datadir)/doc/$(PACKAGE)-$(VERSION)

### all of the static docs, commited to SVN and included as is
static_docs = \
	rfc5215.xml \
	rfc5215.txt \
	eightphase.png \
	evenlsp.png \
	fish_xiph_org.png \
	floor1_inverse_dB_table.html \
	floorval.png \
	fourphase.png \
	framing.html \
	helper.html \
	index.html \
	lspmap.png \
	oddlsp.png \
	oggstream.html \
	programming.html \
	squarepolar.png \
	stereo.html \
	stream.png \
	v-comment.html \
	vorbis-clip.txt \
	vorbis-errors.txt \
	vorbis-fidelity.html \
	vorbis.html \
	vorbisword2.png \
	wait.png \
	white-xifish.png

# bits needed by the spec
SPEC_PNG = \
	components.png \
	floor1-1.png \
	floor1-2.png \
	floor1-3.png \
	floor1-4.png \
	hufftree.png \
	hufftree-under.png \
	residue-pack.png \
	residue2.png \
	window1.png \
	window2.png
SPEC_PDF = xifish.pdf

# FIXME: also needed here
#	white-xifish.png 

SPEC_TEX = \
	Vorbis_I_spec.tex \
	01-introduction.tex \
	02-bitpacking.tex \
	03-codebook.tex \
	04-codec.tex   \
	05-comment.tex \
	06-floor0.tex  \
	07-floor1.tex  \
	08-residue.tex \
	09-helper.tex  \
	10-tables.tex  \
	a1-encapsulation-ogg.tex \
	a2-encapsulation-rtp.tex \
	footer.tex

built_docs = Vorbis_I_spec.pdf Vorbis_I_spec.html Vorbis_I_spec.css

# conditionally make the generated documentation
if BUILD_DOCS
doc_DATA = $(static_docs) $(SPEC_PNG) $(built_docs) doxygen-build.stamp
else
doc_DATA = $(static_docs) doxygen-build.stamp
endif

EXTRA_DIST = $(static_docs) $(built_docs) \
	$(SPEC_TEX) $(SPEC_PNG) $(SPEC_PDF) Vorbis_I_spec.cfg Doxyfile.in

# these are expensive; only remove if we have to
MAINTAINERCLEANFILES = $(built_docs)
CLEANFILES = $(SPEC_TEX:%.tex=%.aux) \
	     Vorbis_I_spec.4ct Vorbis_I_spec.4tc \
	     Vorbis_I_spec.dvi Vorbis_I_spec.idv \
	     Vorbis_I_spec.lg  Vorbis_I_spec.log \
	     Vorbis_I_spec.out Vorbis_I_spec.tmp \
	     Vorbis_I_spec.toc Vorbis_I_spec.xref \
	     Vorbis_I_spec*.png \
	     zzVorbis_I_spec.ps xifish.png
DISTCLEANFILES = $(built_docs)


# explicit rules for generating docs
if BUILD_DOCS
xifish.png: white-xifish.png
	cp $< $@

Vorbis_I_spec.html Vorbis_I_spec.css: $(SPEC_TEX) $(SPEC_PNG) xifish.png
	htlatex $<

Vorbis_I_spec.pdf: $(SPEC_TEX) $(SPEC_PNG) xifish.png
	pdflatex $<
	pdflatex $<
	pdflatex $<
else
Vorbis_I_spec.html: NO_DOCS_ERROR
Vorbis_I_spec.pdf: NO_DOCS_ERROR
NO_DOCS_ERROR:
	@echo
	@echo "*** Documentation has not been built! ***"
	@echo "Try re-running after passing --enable-docs to configure."
	@echo
endif

if HAVE_DOXYGEN
doxygen-build.stamp: Doxyfile $(top_srcdir)/include/vorbis/*.h
	doxygen
	touch doxygen-build.stamp
else
doxygen-build.stamp:
	echo "*** Warning: Doxygen not found; documentation will not be built."
	touch doxygen-build.stamp
endif

install-data-local: doxygen-build.stamp
	$(mkinstalldirs) $(DESTDIR)$(docdir)
	if test -d vorbis; then \
	  for dir in vorbis/*; do \
	    if test -d $$dir; then \
	      b=`basename $$dir`; \
	      $(mkinstalldirs) $(DESTDIR)$(docdir)/$$b; \
	      for f in $$dir/*; do \
		$(INSTALL_DATA) $$f $(DESTDIR)$(docdir)/$$b; \
	      done \
	    fi \
	  done \
	fi

uninstall-local:
	rm -rf $(DESTDIR)$(docdir)

clean-local:
	if test -d vorbis; then rm -rf vorbis; fi
	if test -f doxygen-build.stamp; then rm -f doxygen-build.stamp; fi


