## Process this file with automake to produce Makefile.in
# Makefile(.in) for portSMF using Autotools (but not libtool).
# Written by Richard Ash following Gary Vaughan's Autobook

NYQUIST = nyquist
PORTAUDIO_CFLAGS = -I../portaudio-v19/include
SNDFILE_CFLAGS = -I../libsndfile/src

# tell aclocal that extra macros can be found in autotools/m4/
ACLOCAL_AMFLAGS = -I autotools/m4

INC = \
   -I. \
   -I$(NYQUIST)/nyqsrc \
   -I$(NYQUIST)/tran \
   -I$(NYQUIST)/xlisp \
   -I$(NYQUIST)/sys/unix \
   -I$(NYQUIST)/cmt \
   -I$(NYQUIST)/ffts/src \
   -I$(NYQUIST)/nyqstk/include \
   -I$(NYQUIST)/nyqstk

AM_CFLAGS = $(INC) $(PORTAUDIO_CFLAGS) $(SNDFILE_CFLAGS) \
   -DCMTSTUFF -DEXT

AM_CXXFLAGS = $(INC) $(PORTAUDIO_CFLAGS) $(SNDFILE_CFLAGS) \
   -DCMTSTUFF -DEXT

# define the output library (link with -lportSMF)
lib_LIBRARIES = libnyquist.a

# define the source files to use
libnyquist_a_SOURCES = \
   $(NYQUIST)/xlisp/extern.c \
   $(NYQUIST)/xlisp/xldmem.c \
   $(NYQUIST)/xlisp/xlbfun.c \
   $(NYQUIST)/xlisp/xlcont.c \
   $(NYQUIST)/xlisp/xldbug.c \
   $(NYQUIST)/xlisp/xleval.c \
   $(NYQUIST)/xlisp/xlfio.c \
   $(NYQUIST)/xlisp/xlftab.c \
   $(NYQUIST)/xlisp/xlglob.c \
   $(NYQUIST)/xlisp/xlimage.c \
   $(NYQUIST)/xlisp/xlinit.c \
   $(NYQUIST)/xlisp/xlio.c \
   $(NYQUIST)/xlisp/xlisp.c \
   $(NYQUIST)/xlisp/xljump.c \
   $(NYQUIST)/xlisp/xllist.c \
   $(NYQUIST)/xlisp/xlmath.c \
   $(NYQUIST)/xlisp/xlobj.c \
   $(NYQUIST)/xlisp/xlpp.c \
   $(NYQUIST)/xlisp/xlprin.c \
   $(NYQUIST)/xlisp/xlread.c \
   $(NYQUIST)/xlisp/xlstr.c \
   $(NYQUIST)/xlisp/xlsubr.c \
   $(NYQUIST)/xlisp/xlsym.c \
   $(NYQUIST)/xlisp/xlsys.c \
   $(NYQUIST)/xlisp/path.c \
   $(NYQUIST)/tran/abs.c \
   $(NYQUIST)/tran/allpoles.c \
   $(NYQUIST)/tran/alpass.c \
   $(NYQUIST)/tran/alpasscv.c \
   $(NYQUIST)/tran/alpassvv.c \
   $(NYQUIST)/tran/amosc.c \
   $(NYQUIST)/tran/areson.c \
   $(NYQUIST)/tran/aresonvc.c \
   $(NYQUIST)/tran/aresoncv.c \
   $(NYQUIST)/tran/aresonvv.c \
   $(NYQUIST)/tran/atone.c \
   $(NYQUIST)/tran/atonev.c \
   $(NYQUIST)/tran/biquadfilt.c \
   $(NYQUIST)/tran/buzz.c \
   $(NYQUIST)/tran/chase.c \
   $(NYQUIST)/tran/clip.c \
   $(NYQUIST)/tran/congen.c \
   $(NYQUIST)/tran/const.c \
   $(NYQUIST)/tran/coterm.c \
   $(NYQUIST)/tran/delaycc.c \
   $(NYQUIST)/tran/delaycv.c \
   $(NYQUIST)/tran/eqbandvvv.c \
   $(NYQUIST)/tran/exp.c \
   $(NYQUIST)/tran/follow.c \
   $(NYQUIST)/tran/fmosc.c \
   $(NYQUIST)/tran/fromobject.c \
   $(NYQUIST)/tran/fromarraystream.c \
   $(NYQUIST)/tran/gate.c \
   $(NYQUIST)/tran/ifft.c \
   $(NYQUIST)/tran/instrclar.c \
   $(NYQUIST)/tran/instrclarall.c \
   $(NYQUIST)/tran/instrclarfreq.c \
   $(NYQUIST)/tran/instrsax.c \
   $(NYQUIST)/tran/instrsaxall.c \
   $(NYQUIST)/tran/instrsaxfreq.c \
   $(NYQUIST)/tran/integrate.c \
   $(NYQUIST)/tran/log.c \
   $(NYQUIST)/tran/lpreson.c \
   $(NYQUIST)/tran/maxv.c \
   $(NYQUIST)/tran/offset.c \
   $(NYQUIST)/tran/oneshot.c \
   $(NYQUIST)/tran/osc.c \
   $(NYQUIST)/tran/partial.c \
   $(NYQUIST)/tran/pluck.c \
   $(NYQUIST)/tran/prod.c \
   $(NYQUIST)/tran/pwl.c \
   $(NYQUIST)/tran/quantize.c \
   $(NYQUIST)/tran/recip.c \
   $(NYQUIST)/tran/reson.c \
   $(NYQUIST)/tran/resonvc.c \
   $(NYQUIST)/tran/resoncv.c \
   $(NYQUIST)/tran/resonvv.c \
   $(NYQUIST)/tran/sampler.c \
   $(NYQUIST)/tran/scale.c \
   $(NYQUIST)/tran/shape.c \
   $(NYQUIST)/tran/sine.c \
   $(NYQUIST)/tran/siosc.c \
   $(NYQUIST)/tran/slope.c \
   $(NYQUIST)/tran/sqrt.c \
   $(NYQUIST)/tran/tapf.c \
   $(NYQUIST)/tran/tapv.c \
   $(NYQUIST)/tran/tone.c \
   $(NYQUIST)/tran/tonev.c \
   $(NYQUIST)/tran/upsample.c \
   $(NYQUIST)/tran/white.c \
   $(NYQUIST)/tran/stkrev.c \
   $(NYQUIST)/tran/stkpitshift.c \
   $(NYQUIST)/tran/stkchorus.c \
   $(NYQUIST)/tran/instrbow.c \
   $(NYQUIST)/tran/instrbowedfreq.c \
   $(NYQUIST)/tran/instrbanded.c \
   $(NYQUIST)/tran/instrmandolin.c \
   $(NYQUIST)/tran/instrsitar.c \
   $(NYQUIST)/tran/instrmodalbar.c \
   $(NYQUIST)/tran/instrflute.c \
   $(NYQUIST)/tran/instrflutefreq.c \
   $(NYQUIST)/tran/instrfluteall.c \
   $(NYQUIST)/tran/fmfb.c \
   $(NYQUIST)/tran/fmfbv.c \
   $(NYQUIST)/cmt/cext.c \
   $(NYQUIST)/cmt/cleanup.c \
   $(NYQUIST)/cmt/cmdline.c \
   $(NYQUIST)/cmt/cmtcmd.c \
   $(NYQUIST)/cmt/moxc.c \
   $(NYQUIST)/cmt/mem.c \
   $(NYQUIST)/cmt/midifile.c \
   $(NYQUIST)/cmt/midifns.c \
   $(NYQUIST)/cmt/record.c \
   $(NYQUIST)/cmt/seq.c \
   $(NYQUIST)/cmt/seqmread.c \
   $(NYQUIST)/cmt/seqmwrite.c \
   $(NYQUIST)/cmt/seqread.c \
   $(NYQUIST)/cmt/seqwrite.c \
   $(NYQUIST)/cmt/tempomap.c \
   $(NYQUIST)/cmt/timebase.c \
   $(NYQUIST)/cmt/userio.c \
   $(NYQUIST)/nyqsrc/debug.c \
   $(NYQUIST)/nyqsrc/falloc.c \
   $(NYQUIST)/nyqsrc/local.c \
   $(NYQUIST)/nyqsrc/handlers.c \
   $(NYQUIST)/nyqsrc/multiread.c \
   $(NYQUIST)/nyqsrc/seqext.c \
   $(NYQUIST)/nyqsrc/seqinterf.c \
   $(NYQUIST)/nyqsrc/stats.c \
   $(NYQUIST)/nyqsrc/ffilterkit.c \
   $(NYQUIST)/nyqsrc/sliders.c \
   $(NYQUIST)/nyqsrc/sound.c \
   $(NYQUIST)/nyqsrc/add.c \
   $(NYQUIST)/nyqsrc/avg.c \
   $(NYQUIST)/nyqsrc/compose.c \
   $(NYQUIST)/nyqsrc/convolve.c \
   $(NYQUIST)/nyqsrc/downsample.c \
   $(NYQUIST)/nyqsrc/fft.c \
   $(NYQUIST)/nyqsrc/inverse.c \
   $(NYQUIST)/nyqsrc/multiseq.c \
   $(NYQUIST)/nyqsrc/resamp.c \
   $(NYQUIST)/nyqsrc/resampv.c \
   $(NYQUIST)/nyqsrc/samples.c \
   $(NYQUIST)/nyqsrc/sndmax.c \
   $(NYQUIST)/nyqsrc/sndread.c \
   $(NYQUIST)/nyqsrc/sndseq.c \
   $(NYQUIST)/nyqsrc/sndwritepa.c \
   $(NYQUIST)/nyqsrc/yin.c \
   $(NYQUIST)/nyqsrc/trigger.c \
   $(NYQUIST)/nyqsrc/lpanal.c \
   $(NYQUIST)/nyqstk/src/Generator.cpp \
   $(NYQUIST)/nyqstk/src/SineWave.cpp \
   $(NYQUIST)/nyqstk/src/Function.cpp \
   $(NYQUIST)/nyqstk/src/FileRead.cpp \
   $(NYQUIST)/nyqstk/src/FileWvIn.cpp \
   $(NYQUIST)/nyqstk/src/Effect.cpp \
   $(NYQUIST)/nyqstk/src/Clarinet.cpp \
   $(NYQUIST)/nyqstk/src/Delay.cpp \
   $(NYQUIST)/nyqstk/src/DelayL.cpp \
   $(NYQUIST)/nyqstk/src/Envelope.cpp \
   $(NYQUIST)/nyqstk/src/Filter.cpp \
   $(NYQUIST)/nyqstk/src/Instrmnt.cpp \
   $(NYQUIST)/nyqstk/src/Noise.cpp \
   $(NYQUIST)/nyqstk/src/OneZero.cpp \
   $(NYQUIST)/nyqstk/src/ReedTable.cpp \
   $(NYQUIST)/nyqstk/src/Saxofony.cpp \
   $(NYQUIST)/nyqstk/src/Stk.cpp \
   $(NYQUIST)/nyqstk/src/WaveLoop.cpp \
   $(NYQUIST)/nyqstk/src/WvIn.cpp \
   $(NYQUIST)/nyqstk/src/NRev.cpp \
   $(NYQUIST)/nyqstk/src/JCRev.cpp \
   $(NYQUIST)/nyqstk/src/PRCRev.cpp \
   $(NYQUIST)/nyqstk/src/PitShift.cpp \
   $(NYQUIST)/nyqstk/src/Chorus.cpp \
   $(NYQUIST)/nyqstk/src/Bowed.cpp \
   $(NYQUIST)/nyqstk/src/BowTable.cpp \
   $(NYQUIST)/nyqstk/src/ADSR.cpp \
   $(NYQUIST)/nyqstk/src/OnePole.cpp \
   $(NYQUIST)/nyqstk/src/BiQuad.cpp \
   $(NYQUIST)/nyqstk/src/BandedWG.cpp \
   $(NYQUIST)/nyqstk/src/DelayA.cpp \
   $(NYQUIST)/nyqstk/src/Mandolin.cpp \
   $(NYQUIST)/nyqstk/src/PluckTwo.cpp \
   $(NYQUIST)/nyqstk/src/Sitar.cpp \
   $(NYQUIST)/nyqstk/src/ModalBar.cpp \
   $(NYQUIST)/nyqstk/src/Modal.cpp \
   $(NYQUIST)/nyqstk/src/Flute.cpp \
   $(NYQUIST)/nyqstk/src/JetTable.cpp \
   $(NYQUIST)/nyqstk/src/PoleZero.cpp \
   $(NYQUIST)/nyqstk/stkinit.cpp \
   $(NYQUIST)/nyqstk/instr.cpp \
   $(NYQUIST)/nyqstk/stkint.cpp \
   $(NYQUIST)/ffts/src/fftext.c \
   $(NYQUIST)/ffts/src/fftlib.c \
   $(NYQUIST)/ffts/src/matlib.c \
   $(NYQUIST)/nyqsrc/sndfnint.c \
   $(NYQUIST)/nyqsrc/seqfnint.c \
   nyx.c

# files that only really viscous cleans remove
MAINTAINERCLEANFILES = Makefile.in

# other distributed files in case users don't have a complete set of auto*
AUX_DIST = $(ac_aux_dir)/config.guess \
   $(ac_aux_dir)/config.sub \
   $(ac_aux_dir)/install-sh \
   $(ac_aux_dir)/mdate-sh \
   $(ac_aux_dir)/missing \
   $(ac_aux_dir)/mkinstalldirs \
   $(MAINTAINERCLEANFILES)
