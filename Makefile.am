SUBDIRS = help images lib-src po src tests

ACLOCAL_AMFLAGS = -I m4

bin_PROGRAMS = audacity$(EXEEXT)

dist_doc_DATA = LICENSE.txt README.txt
dist_pkgdata_DATA = presets/EQDefaultCurves.xml

nobase_dist_pkgdata_DATA = \
   include/audacity/ConfigInterface.h \
   include/audacity/EffectAutomationParameters.h \
   include/audacity/EffectInterface.h \
   include/audacity/IdentInterface.h \
   include/audacity/ImporterInterface.h \
   include/audacity/ModuleInterface.h \
   include/audacity/PluginInterface.h \
   include/audacity/Types.h \
   nyquist/dspprims.lsp \
   nyquist/envelopes.lsp \
   nyquist/equalizer.lsp \
   nyquist/evalenv.lsp \
   nyquist/fileio.lsp \
   nyquist/follow.lsp \
   nyquist/init.lsp \
   nyquist/misc.lsp \
   nyquist/nyinit.lsp \
   nyquist/nyqmisc.lsp \
   nyquist/nyquist-plot.txt \
   nyquist/nyquist.lsp \
   nyquist/printrec.lsp \
   nyquist/profile.lsp \
   nyquist/sal-parse.lsp \
   nyquist/sal.lsp \
   nyquist/seq.lsp \
   nyquist/seqfnint.lsp \
   nyquist/seqmidi.lsp \
   nyquist/sndfnint.lsp \
   nyquist/stk.lsp \
   nyquist/system.lsp \
   nyquist/test.lsp \
   nyquist/upic.sal \
   nyquist/velocity.lsp \
   nyquist/xlinit.lsp \
   nyquist/xm.lsp \
   nyquist/rawwaves/mand1.raw \
   nyquist/rawwaves/mand2.raw \
   nyquist/rawwaves/mand3.raw \
   nyquist/rawwaves/mand4.raw \
   nyquist/rawwaves/mand5.raw \
   nyquist/rawwaves/mand6.raw \
   nyquist/rawwaves/mand7.raw \
   nyquist/rawwaves/mand8.raw \
   nyquist/rawwaves/mand9.raw \
   nyquist/rawwaves/mand10.raw \
   nyquist/rawwaves/mand11.raw \
   nyquist/rawwaves/mand12.raw \
   nyquist/rawwaves/mandpluk.raw \
   nyquist/rawwaves/marmstk1.raw \
   nyquist/rawwaves/sinewave.raw \
   plug-ins/adjustable-fade.ny \
   plug-ins/beat.ny \
   plug-ins/clicktrack.ny \
   plug-ins/clipfix.ny \
   plug-ins/crossfadeclips.ny \
   plug-ins/crossfadetracks.ny \
   plug-ins/delay.ny \
   plug-ins/equalabel.ny \
   plug-ins/highpass.ny \
   plug-ins/limiter.ny \
   plug-ins/lowpass.ny \
   plug-ins/notch.ny \
   plug-ins/pluck.ny \
   plug-ins/rissetdrum.ny \
   plug-ins/sample-data-export.ny \
   plug-ins/SilenceMarker.ny \
   plug-ins/SoundFinder.ny \
   plug-ins/SpectralEditMulti.ny \
   plug-ins/SpectralEditParametricEQ.ny \
   plug-ins/SpectralEditShelves.ny \
   plug-ins/StudioFadeOut.ny \
   plug-ins/tremolo.ny \
   plug-ins/vocalremover.ny \
   plug-ins/vocalrediso.ny \
   plug-ins/vocoder.ny \
   $(NULL)

audacity$(EXEEXT): src/audacity$(EXEEXT)
	rm -f audacity$(EXEEXT)
	cp src/audacity$(EXEEXT) audacity$(EXEEXT)
	[ -e plug-ins ] || ln -s $(srcdir)/plug-ins
	[ -e nyquist ] || ln -s $(srcdir)/nyquist
	[ -e locale ] || ln -s po locale

modules:
	mkdir -p modules
	(cd lib-src/mod-nyq-bench && make clean && make && cp .libs/mod-nyq-bench.so ../../modules)

.PHONY: modules

CLEANFILES = audacity$(EXEEXT) plug-ins nyquist locale modules
