#
# Makefile for Nyquist, SYSTEM-TYPE is ALSA
# run make in the top-level Nyquist directory to compile Nyquist
#
# NOTE: this file is machine-generated.  DO NOT EDIT!
#   Instead, modify makefile.lsp and regenerate the makefile.
#   Ports and bug fixes are welcome - please mail them to 
#   dannenberg@cs.cmu.edu.  Thanks.
#

# This is the resulting executable (normally "ny"):
NY = ny

OPT = -O2 -m32
# OPT = -g -m32

EVERYTHING = $(NY) runtime/system.lsp jnyqide/jNyqIDE.jar \
           bin/ser-to-osc bin/test-client

CURRENT = $(EVERYTHING)

current: $(CURRENT)

onlyny: $(NY) runtime/system.lsp

JAVASRC =  jnyqide/browser.java jnyqide/NyquistThread.java \
	jnyqide/Pair.java jnyqide/BareBonesBrowserLaunch.java \
	jnyqide/EnvelopeFrame.java jnyqide/Piano_Roll.java \
	jnyqide/FindDialog.java jnyqide/PlotFrame.java \
	jnyqide/InstrumentCharacteristics.java jnyqide/PlotMouseAdapter.java \
	jnyqide/Jslide.java jnyqide/PopupListener.java \
	jnyqide/LispFileFilter.java jnyqide/PreferencesDialog.java \
	jnyqide/MainFrame_AboutBox.java jnyqide/ReplaceDialog.java \
	jnyqide/MainFrame.java jnyqide/SpringUtilities.java jnyqide/Main.java \
	jnyqide/NotFoundDialog.java jnyqide/TextColor.java jnyqide/NyqPlot.java \
	jnyqide/Trie.java jnyqide/NyquistFile.java jnyqide/WordList.java

jnyqide/jNyqIDE.jar: $(JAVASRC)
	if [ -r jnyqide/SpecialMacHandler.java ] ; then \
	mv jnyqide/SpecialMacHandler.java jnyqide/SpecialMacHandler.hidden ;\
	fi 
	cd jnyqide; javac *.java 
	mv jnyqide/SpecialMacHandler.hidden jnyqide/SpecialMacHandler.java 
	rm -rf jnyqide/jNyqIDE.jar 
	jar -cfm jnyqide/jNyqIDE.jar jnyqide/manifest.txt jnyqide/*.class 

# Standard list of includes (common to all unix versions)
# Keeping portaudio and libsndfile sources local to nyquist
INCL = -Inyqsrc -Itran -Ixlisp -Isys/unix -Icmt -Iffts/src \
   -Inyqstk/include -Inyqstk -Iportaudio/include -Iportaudio/src/common \
   -Iportaudio/src/os/unix \
   -Iliblo -Inylsf -IFLAC/include -Ilibogg/include -Ilibvorbis/include

# system dependent stuff for alsa:

AUDIOLIBS = -lasound

CC = gcc

LIBPA_PATH = portaudio/lib/.libs
LIBOGG_PATH = libogg/src/.libs
LIBVORBIS_PATH = libvorbis/lib/.libs
LIBLO_PATH = liblo/src/.libs
LIBFLAC_PATH = FLAC/obj/release/lib

# to enable command line editing, use -DREADLINE. WARNING: THIS WILL 
# DISABLE THE ABILITY TO INTERRUPT LISP AND USE SOME OTHER HANDY 
# CONTROL CHARACTERS (You will also need the readline and curses libraries)
CFLAGS = -DOSC -DCMTSTUFF $(OPT) $(INCL) \
    -DHAVE_LIBPTHREAD=1 -D_FILE_OFFSET_BITS=64 \
    -DSTK_NYQUIST -DUSE_VSPRINTF \
    -DHAVE_CONFIG_H
LN = g++ -m32
AR = ar
# to enable command line editing, insert -lreadline -lcurses
LFLAGS = -L/usr/lib32 $(LIBPA_PATH)/libportaudio.a \
         $(LIBLO_PATH)/liblo.a $(AUDIOLIBS) $(LIBFLAC_PATH)/libFLAC.a \
         $(LIBVORBIS_PATH)/libvorbis.a \
         $(LIBVORBIS_PATH)/libvorbisfile.a \
         $(LIBVORBIS_PATH)/libvorbisenc.a $(LIBOGG_PATH)/libogg.a \
         -lm -lpthread -lrt

TAGS:
	find . ( -name "*.c" -o -name "*.h" ) -print | etags -

tags: TAGS

# end of system dependencies

INTGEN = misc/intgen

# Object files for Nyquist:
OBJECTS =  xlisp/extern.o xlisp/xldmem.o xlisp/xlbfun.o xlisp/xlcont.o \
	xlisp/xldbug.o xlisp/xleval.o xlisp/xlfio.o xlisp/xlftab.o \
	xlisp/xlglob.o xlisp/xlimage.o xlisp/xlinit.o xlisp/xlio.o \
	xlisp/xlisp.o xlisp/xljump.o xlisp/xllist.o xlisp/xlmath.o \
	xlisp/xlobj.o xlisp/xlpp.o xlisp/xlprin.o xlisp/xlread.o xlisp/xlstr.o \
	xlisp/xlsubr.o xlisp/xlsym.o xlisp/xlsys.o xlisp/path.o tran/abs.o \
	tran/allpoles.o tran/alpass.o tran/alpasscv.o tran/alpassvv.o \
	tran/amosc.o tran/areson.o tran/aresonvc.o tran/aresoncv.o \
	tran/aresonvv.o tran/atone.o tran/atonev.o tran/biquadfilt.o \
	tran/buzz.o tran/chase.o tran/clip.o tran/congen.o tran/const.o \
	tran/coterm.o tran/delaycc.o tran/delaycv.o tran/eqbandvvv.o tran/exp.o \
	tran/follow.o tran/fmosc.o tran/fromobject.o tran/fromarraystream.o \
	tran/gate.o tran/ifft.o tran/instrclar.o tran/instrclarall.o \
	tran/instrclarfreq.o tran/instrsax.o tran/instrsaxall.o \
	tran/instrsaxfreq.o tran/integrate.o tran/log.o tran/lpreson.o \
	tran/maxv.o tran/offset.o tran/oneshot.o tran/osc.o tran/partial.o \
	tran/pluck.o tran/prod.o tran/pwl.o tran/quantize.o tran/recip.o \
	tran/reson.o tran/resonvc.o tran/resoncv.o tran/resonvv.o \
	tran/sampler.o tran/scale.o tran/shape.o tran/sine.o tran/siosc.o \
	tran/slope.o tran/sqrt.o tran/tapf.o tran/tapv.o tran/tone.o \
	tran/tonev.o tran/upsample.o tran/white.o tran/stkrev.o \
	tran/stkpitshift.o tran/stkchorus.o tran/instrbow.o \
	tran/instrbowedfreq.o tran/instrbanded.o tran/instrmandolin.o \
	tran/instrsitar.o tran/instrmodalbar.o tran/instrflute.o \
	tran/instrflutefreq.o tran/instrfluteall.o tran/fmfb.o tran/fmfbv.o \
	cmt/cext.o cmt/cleanup.o cmt/cmdline.o cmt/cmtcmd.o cmt/moxc.o \
	cmt/mem.o cmt/midifile.o cmt/midifns.o cmt/record.o cmt/seq.o \
	cmt/seqmread.o cmt/seqmwrite.o cmt/seqread.o cmt/seqwrite.o \
	cmt/tempomap.o cmt/timebase.o cmt/userio.o nylsf/aiff.o nylsf/alaw.o \
	nylsf/au.o nylsf/audio_detect.o nylsf/avr.o nylsf/broadcast.o \
	nylsf/caf.o nylsf/chanmap.o nylsf/chunk.o nylsf/command.o \
	nylsf/common.o nylsf/dither.o nylsf/double64.o nylsf/dwd.o nylsf/dwvw.o \
	nylsf/file_io.o nylsf/flac.o nylsf/float32.o nylsf/gsm610.o nylsf/htk.o \
	nylsf/id3.o nylsf/ima_adpcm.o nylsf/ima_oki_adpcm.o nylsf/interleave.o \
	nylsf/ircam.o nylsf/macbinary3.o nylsf/macos.o nylsf/mat4.o \
	nylsf/mat5.o nylsf/mpc2k.o nylsf/ms_adpcm.o nylsf/nist.o nylsf/ogg.o \
	nylsf/ogg_vorbis.o nylsf/paf.o nylsf/pcm.o nylsf/pvf.o nylsf/raw.o \
	nylsf/rf64.o nylsf/rx2.o nylsf/sd2.o nylsf/sds.o nylsf/sndfile.o \
	nylsf/strings.o nylsf/svx.o nylsf/txw.o nylsf/ulaw.o nylsf/voc.o \
	nylsf/vox_adpcm.o nylsf/w64.o nylsf/wav.o nylsf/wav_w64.o nylsf/wve.o \
	nylsf/xi.o nylsf/g72x.o nylsf/GSM610/add.o nylsf/GSM610/code.o \
	nylsf/GSM610/decode.o nylsf/GSM610/gsm_create.o \
	nylsf/GSM610/gsm_decode.o nylsf/GSM610/gsm_destroy.o \
	nylsf/GSM610/gsm_encode.o nylsf/GSM610/gsm_option.o \
	nylsf/GSM610/long_term.o nylsf/GSM610/lpc.o nylsf/GSM610/preprocess.o \
	nylsf/GSM610/rpe.o nylsf/GSM610/short_term.o nylsf/GSM610/table.o \
	nylsf/G72x/g721.o nylsf/G72x/g723_16.o nylsf/G72x/g723_24.o \
	nylsf/G72x/g723_40.o nylsf/G72x/g72x.o nyqsrc/debug.o nyqsrc/falloc.o \
	nyqsrc/local.o nyqsrc/handlers.o nyqsrc/multiread.o nyqsrc/seqext.o \
	nyqsrc/seqinterf.o nyqsrc/stats.o nyqsrc/ffilterkit.o nyqsrc/sound.o \
	nyqsrc/add.o nyqsrc/avg.o nyqsrc/compose.o nyqsrc/convolve.o \
	nyqsrc/downsample.o nyqsrc/fft.o nyqsrc/inverse.o nyqsrc/multiseq.o \
	nyqsrc/resamp.o nyqsrc/resampv.o nyqsrc/samples.o nyqsrc/sndmax.o \
	nyqsrc/sndread.o nyqsrc/sndseq.o nyqsrc/sndsliders.o \
	nyqsrc/sliderdata.o nyqsrc/sndwritepa.o nyqsrc/yin.o \
	nyqsrc/nyq-osc-server.o nyqsrc/trigger.o nyqsrc/lpanal.o \
	nyqsrc/phasevocoder.o nyqsrc/pvshell.o nyqstk/src/Generator.o \
	nyqstk/src/SineWave.o nyqstk/src/Function.o nyqstk/src/FileRead.o \
	nyqstk/src/FileWvIn.o nyqstk/src/Effect.o nyqstk/src/Clarinet.o \
	nyqstk/src/Delay.o nyqstk/src/DelayL.o nyqstk/src/Envelope.o \
	nyqstk/src/Filter.o nyqstk/src/Instrmnt.o nyqstk/src/Noise.o \
	nyqstk/src/OneZero.o nyqstk/src/ReedTable.o nyqstk/src/Saxofony.o \
	nyqstk/src/Stk.o nyqstk/src/WaveLoop.o nyqstk/src/WvIn.o \
	nyqstk/src/NRev.o nyqstk/src/JCRev.o nyqstk/src/PRCRev.o \
	nyqstk/src/PitShift.o nyqstk/src/Chorus.o nyqstk/src/Bowed.o \
	nyqstk/src/BowTable.o nyqstk/src/ADSR.o nyqstk/src/OnePole.o \
	nyqstk/src/BiQuad.o nyqstk/src/BandedWG.o nyqstk/src/DelayA.o \
	nyqstk/src/Mandolin.o nyqstk/src/PluckTwo.o nyqstk/src/Sitar.o \
	nyqstk/src/ModalBar.o nyqstk/src/Modal.o nyqstk/src/Flute.o \
	nyqstk/src/JetTable.o nyqstk/src/PoleZero.o nyqstk/stkinit.o \
	nyqstk/instr.o nyqstk/stkint.o ffts/src/fftext.o ffts/src/fftlib.o \
	ffts/src/matlib.o nyqsrc/sndfnint.o nyqsrc/seqfnint.o \
	sys/unix/osstuff.o sys/unix/term.o

# Sound functions to add to xlisp
SNDFNINT_HDRS =  nyqsrc/sndfmt.h nylsf/sndfile.h nyqsrc/sound.h \
	nyqsrc/add.h nyqsrc/avg.h nyqsrc/compose.h nyqsrc/convolve.h \
	nyqsrc/downsample.h nyqsrc/fft.h nyqsrc/inverse.h nyqsrc/multiseq.h \
	nyqsrc/resamp.h nyqsrc/resampv.h nyqsrc/samples.h nyqsrc/sndmax.h \
	nyqsrc/sndread.h nyqsrc/sndseq.h nyqsrc/sndsliders.h \
	nyqsrc/sliderdata.h nyqsrc/sndwritepa.h nyqsrc/yin.h \
	nyqsrc/nyq-osc-server.h nyqsrc/trigger.h nyqsrc/lpanal.h \
	nyqsrc/phasevocoder.h nyqsrc/pvshell.h tran/abs.h tran/allpoles.h \
	tran/alpass.h tran/alpasscv.h tran/alpassvv.h tran/amosc.h \
	tran/areson.h tran/aresonvc.h tran/aresoncv.h tran/aresonvv.h \
	tran/atone.h tran/atonev.h tran/biquadfilt.h tran/buzz.h tran/chase.h \
	tran/clip.h tran/congen.h tran/const.h tran/coterm.h tran/delaycc.h \
	tran/delaycv.h tran/eqbandvvv.h tran/exp.h tran/follow.h tran/fmosc.h \
	tran/fromobject.h tran/fromarraystream.h tran/gate.h tran/ifft.h \
	tran/instrclar.h tran/instrclarall.h tran/instrclarfreq.h \
	tran/instrsax.h tran/instrsaxall.h tran/instrsaxfreq.h tran/integrate.h \
	tran/log.h tran/lpreson.h tran/maxv.h tran/offset.h tran/oneshot.h \
	tran/osc.h tran/partial.h tran/pluck.h tran/prod.h tran/pwl.h \
	tran/quantize.h tran/recip.h tran/reson.h tran/resonvc.h tran/resoncv.h \
	tran/resonvv.h tran/sampler.h tran/scale.h tran/shape.h tran/sine.h \
	tran/siosc.h tran/slope.h tran/sqrt.h tran/tapf.h tran/tapv.h \
	tran/tone.h tran/tonev.h tran/upsample.h tran/white.h tran/stkrev.h \
	tran/stkpitshift.h tran/stkchorus.h tran/instrbow.h \
	tran/instrbowedfreq.h tran/instrbanded.h tran/instrmandolin.h \
	tran/instrsitar.h tran/instrmodalbar.h tran/instrflute.h \
	tran/instrflutefreq.h tran/instrfluteall.h tran/fmfb.h tran/fmfbv.h \
	nyqsrc/sndwrite.h

SEQFNINT_HDRS =  cmt/seqdecls.h nyqsrc/seqext.h cmt/seq.h \
	nyqsrc/seqinterf.h cmt/seqread.h cmt/seqmread.h cmt/seqwrite.h \
	cmt/seqmwrite.h



liblo/Makefile:
	cd liblo; ./configure CFLAGS=-m32 LDFLAGS=-m32 CXXFLAGS=-m32 --enable-static --disable-shared
	# sometimes, residual files cause problems
	cd liblo; make clean

$(LIBLO_PATH)/liblo.a: liblo/Makefile
	cd liblo; make

bin/ser-to-osc: $(LIBLO_PATH)/liblo.a
	mkdir -p bin
	$(CC) -c $(CFLAGS) liblo/ser-to-osc/ser-to-osc.cpp \
	      -o liblo/ser-to-osc/ser-to-osc.o
	$(LN)  liblo/ser-to-osc/ser-to-osc.o -o bin/ser-to-osc $(LFLAGS)

bin/test-client: $(LIBLO_PATH)/liblo.a
	mkdir -p bin
	$(CC) -c $(CFLAGS) liblo/test-client/test-client.c \
	      -o liblo/test-client/test-client.o
	$(LN) liblo/test-client/test-client.o -o bin/test-client  $(LFLAGS)

portaudio/Makefile:
	# note: without-jack avoids 32/64-bit link error on Debian
	cd portaudio; ./configure CFLAGS=-m32 LDFLAGS=-m32 CXXFLAGS=-m32 --enable-static --disable-shared --without-jack --with-alsa
	# sometimes, residual files cause problems
	cd portaudio; make clean

$(LIBPA_PATH)/libportaudio.a: portaudio/Makefile
	cd portaudio; make lib/libportaudio.la

libogg/Makefile:
	cd libogg; ./configure CFLAGS=-m32 LDFLAGS=-m32 CXXFLAGS=-m32 --enable-static --disable-shared

$(LIBOGG_PATH)/libogg.a: libogg/Makefile
	cd libogg; make

# NOTE: libvorbis/configure on a 64-bit machine will expect to find
#  libogg installed (even though we are not going to use the installed
#  libogg library, and even though the installed libogg library will
#  be for 64-bit architecture. This represents a bug in configure
#  because it checks for a 64-bit library when it is building for
#  a 32-bit architecture (we pass in CFLAGS=-m32). In spite of the 
#  bug, configure will build a Makefile that will build a 32-bit
#  libvorbis library that we need. We will link it with other 
#  32-bit code including 32-bit libogg.a

libvorbis/Makefile:
	cd libvorbis; ./configure CFLAGS=-m32 LDFLAGS=-m32 CXXFLAGS=-m32 --enable-static --disable-shared

$(LIBVORBIS_PATH)/libvorbis.a: libvorbis/Makefile
	cd libvorbis; make

$(LIBVORBIS_PATH)/libvorbisfile.a: libvorbis/Makefile
	cd libvorbis; make

$(LIBVORBIS_PATH)/libvorbisenc.a: libvorbis/Makefile
	cd libvorbis; make


$(LIBFLAC_PATH)/libFLAC.a: FLAC/src/libFLAC/Makefile.lite
	cd FLAC/src/libFLAC; make -f Makefile.lite

$(NY): $(OBJECTS) $(LIBPA_PATH)/libportaudio.a $(LIBLO_PATH)/liblo.a \
       FLAC/obj/release/lib/libFLAC.a \
       $(LIBVORBIS_PATH)/libvorbis.a $(LIBVORBIS_PATH)/libvorbisfile.a \
       $(LIBVORBIS_PATH)/libvorbisenc.a $(LIBOGG_PATH)/libogg.a
	$(LN) $(OBJECTS) $(LFLAGS) -o $(NY)

# copy appropriate system.lsp and make it read-only;
# changes should be made to sys/unix/<system>/system.lsp
runtime/system.lsp: sys/unix/alsa/system.lsp
#	make sure it's there before you make it writeable
	touch runtime/system.lsp
	chmod +w runtime/system.lsp
	cp -p sys/unix/alsa/system.lsp runtime/system.lsp
	chmod -w runtime/system.lsp

NYQDEP = nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h

nyqsrc/sound.o: nyqsrc/sound.c nyqsrc/sound.h $(NYQDEP)
	$(CC) -c nyqsrc/sound.c -o nyqsrc/sound.o $(CFLAGS)

nyqsrc/add.o: nyqsrc/add.c nyqsrc/add.h $(NYQDEP)
	$(CC) -c nyqsrc/add.c -o nyqsrc/add.o $(CFLAGS)

nyqsrc/avg.o: nyqsrc/avg.c nyqsrc/avg.h $(NYQDEP)
	$(CC) -c nyqsrc/avg.c -o nyqsrc/avg.o $(CFLAGS)

nyqsrc/compose.o: nyqsrc/compose.c nyqsrc/compose.h $(NYQDEP)
	$(CC) -c nyqsrc/compose.c -o nyqsrc/compose.o $(CFLAGS)

nyqsrc/convolve.o: nyqsrc/convolve.c nyqsrc/convolve.h $(NYQDEP)
	$(CC) -c nyqsrc/convolve.c -o nyqsrc/convolve.o $(CFLAGS)

nyqsrc/downsample.o: nyqsrc/downsample.c nyqsrc/downsample.h $(NYQDEP)
	$(CC) -c nyqsrc/downsample.c -o nyqsrc/downsample.o $(CFLAGS)

nyqsrc/fft.o: nyqsrc/fft.c nyqsrc/fft.h $(NYQDEP)
	$(CC) -c nyqsrc/fft.c -o nyqsrc/fft.o $(CFLAGS)

nyqsrc/inverse.o: nyqsrc/inverse.c nyqsrc/inverse.h $(NYQDEP)
	$(CC) -c nyqsrc/inverse.c -o nyqsrc/inverse.o $(CFLAGS)

nyqsrc/multiseq.o: nyqsrc/multiseq.c nyqsrc/multiseq.h $(NYQDEP)
	$(CC) -c nyqsrc/multiseq.c -o nyqsrc/multiseq.o $(CFLAGS)

nyqsrc/resamp.o: nyqsrc/resamp.c nyqsrc/resamp.h $(NYQDEP)
	$(CC) -c nyqsrc/resamp.c -o nyqsrc/resamp.o $(CFLAGS)

nyqsrc/resampv.o: nyqsrc/resampv.c nyqsrc/resampv.h $(NYQDEP)
	$(CC) -c nyqsrc/resampv.c -o nyqsrc/resampv.o $(CFLAGS)

nyqsrc/samples.o: nyqsrc/samples.c nyqsrc/samples.h $(NYQDEP)
	$(CC) -c nyqsrc/samples.c -o nyqsrc/samples.o $(CFLAGS)

nyqsrc/sndmax.o: nyqsrc/sndmax.c nyqsrc/sndmax.h $(NYQDEP)
	$(CC) -c nyqsrc/sndmax.c -o nyqsrc/sndmax.o $(CFLAGS)

nyqsrc/sndread.o: nyqsrc/sndread.c nyqsrc/sndread.h $(NYQDEP)
	$(CC) -c nyqsrc/sndread.c -o nyqsrc/sndread.o $(CFLAGS)

nyqsrc/sndseq.o: nyqsrc/sndseq.c nyqsrc/sndseq.h $(NYQDEP)
	$(CC) -c nyqsrc/sndseq.c -o nyqsrc/sndseq.o $(CFLAGS)

nyqsrc/sndsliders.o: nyqsrc/sndsliders.c nyqsrc/sndsliders.h $(NYQDEP)
	$(CC) -c nyqsrc/sndsliders.c -o nyqsrc/sndsliders.o $(CFLAGS)

nyqsrc/sliderdata.o: nyqsrc/sliderdata.c nyqsrc/sliderdata.h $(NYQDEP)
	$(CC) -c nyqsrc/sliderdata.c -o nyqsrc/sliderdata.o $(CFLAGS)

nyqsrc/sndwritepa.o: nyqsrc/sndwritepa.c nyqsrc/sndwrite.h
	$(CC) -c nyqsrc/sndwritepa.c -o nyqsrc/sndwritepa.o $(CFLAGS)

nyqsrc/yin.o: nyqsrc/yin.c nyqsrc/yin.h $(NYQDEP)
	$(CC) -c nyqsrc/yin.c -o nyqsrc/yin.o $(CFLAGS)

nyqsrc/nyq-osc-server.o: nyqsrc/nyq-osc-server.c nyqsrc/nyq-osc-server.h $(NYQDEP)
	$(CC) -c nyqsrc/nyq-osc-server.c -o nyqsrc/nyq-osc-server.o $(CFLAGS)

nyqsrc/trigger.o: nyqsrc/trigger.c nyqsrc/trigger.h $(NYQDEP)
	$(CC) -c nyqsrc/trigger.c -o nyqsrc/trigger.o $(CFLAGS)

nyqsrc/lpanal.o: nyqsrc/lpanal.c nyqsrc/lpanal.h $(NYQDEP)
	$(CC) -c nyqsrc/lpanal.c -o nyqsrc/lpanal.o $(CFLAGS)

nyqsrc/phasevocoder.o: nyqsrc/phasevocoder.c nyqsrc/phasevocoder.h $(NYQDEP)
	$(CC) -c nyqsrc/phasevocoder.c -o nyqsrc/phasevocoder.o $(CFLAGS)

nyqsrc/pvshell.o: nyqsrc/pvshell.c nyqsrc/pvshell.h $(NYQDEP)
	$(CC) -c nyqsrc/pvshell.c -o nyqsrc/pvshell.o $(CFLAGS)

nyqsrc/debug.o: nyqsrc/debug.c nyqsrc/debug.h $(NYQDEP)
	$(CC) -c nyqsrc/debug.c -o nyqsrc/debug.o $(CFLAGS)

nyqsrc/falloc.o: nyqsrc/falloc.c nyqsrc/falloc.h $(NYQDEP)
	$(CC) -c nyqsrc/falloc.c -o nyqsrc/falloc.o $(CFLAGS)

nyqsrc/local.o: nyqsrc/local.c xlisp/xlisp.h nyqsrc/sound.h
	$(CC) -c nyqsrc/local.c -o nyqsrc/local.o $(CFLAGS)

nyqsrc/handlers.o: nyqsrc/handlers.c 
	$(CC) -c nyqsrc/handlers.c -o nyqsrc/handlers.o $(CFLAGS)

nyqsrc/multiread.o: nyqsrc/multiread.c nyqsrc/multiread.h $(NYQDEP)
	$(CC) -c nyqsrc/multiread.c -o nyqsrc/multiread.o $(CFLAGS)

nyqsrc/seqext.o: nyqsrc/seqext.c nyqsrc/seqext.h $(NYQDEP)
	$(CC) -c nyqsrc/seqext.c -o nyqsrc/seqext.o $(CFLAGS)

nyqsrc/seqinterf.o: nyqsrc/seqinterf.c nyqsrc/seqinterf.h $(NYQDEP)
	$(CC) -c nyqsrc/seqinterf.c -o nyqsrc/seqinterf.o $(CFLAGS)

nyqsrc/stats.o: nyqsrc/stats.c nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	$(CC) -c nyqsrc/stats.c -o nyqsrc/stats.o $(CFLAGS)

nyqsrc/ffilterkit.o: nyqsrc/ffilterkit.c nyqsrc/ffilterkit.h $(NYQDEP)
	$(CC) -c nyqsrc/ffilterkit.c -o nyqsrc/ffilterkit.o $(CFLAGS)

ffts/src/fftext.o: ffts/src/fftext.c ffts/src/fftext.h $(NYQDEP)
	$(CC) -c ffts/src/fftext.c -o ffts/src/fftext.o $(CFLAGS)

ffts/src/fftlib.o: ffts/src/fftlib.c ffts/src/fftlib.h $(NYQDEP)
	$(CC) -c ffts/src/fftlib.c -o ffts/src/fftlib.o $(CFLAGS)

ffts/src/matlib.o: ffts/src/matlib.c ffts/src/matlib.h $(NYQDEP)
	$(CC) -c ffts/src/matlib.c -o ffts/src/matlib.o $(CFLAGS)

tran/abs.o: tran/abs.c tran/abs.h $(NYQDEP)
	$(CC) -c tran/abs.c -o tran/abs.o $(CFLAGS)

tran/allpoles.o: tran/allpoles.c tran/allpoles.h $(NYQDEP)
	$(CC) -c tran/allpoles.c -o tran/allpoles.o $(CFLAGS)

tran/alpass.o: tran/alpass.c tran/alpass.h $(NYQDEP)
	$(CC) -c tran/alpass.c -o tran/alpass.o $(CFLAGS)

tran/alpasscv.o: tran/alpasscv.c tran/alpasscv.h $(NYQDEP)
	$(CC) -c tran/alpasscv.c -o tran/alpasscv.o $(CFLAGS)

tran/alpassvv.o: tran/alpassvv.c tran/alpassvv.h $(NYQDEP)
	$(CC) -c tran/alpassvv.c -o tran/alpassvv.o $(CFLAGS)

tran/amosc.o: tran/amosc.c tran/amosc.h $(NYQDEP)
	$(CC) -c tran/amosc.c -o tran/amosc.o $(CFLAGS)

tran/areson.o: tran/areson.c tran/areson.h $(NYQDEP)
	$(CC) -c tran/areson.c -o tran/areson.o $(CFLAGS)

tran/aresonvc.o: tran/aresonvc.c tran/aresonvc.h $(NYQDEP)
	$(CC) -c tran/aresonvc.c -o tran/aresonvc.o $(CFLAGS)

tran/aresoncv.o: tran/aresoncv.c tran/aresoncv.h $(NYQDEP)
	$(CC) -c tran/aresoncv.c -o tran/aresoncv.o $(CFLAGS)

tran/aresonvv.o: tran/aresonvv.c tran/aresonvv.h $(NYQDEP)
	$(CC) -c tran/aresonvv.c -o tran/aresonvv.o $(CFLAGS)

tran/atone.o: tran/atone.c tran/atone.h $(NYQDEP)
	$(CC) -c tran/atone.c -o tran/atone.o $(CFLAGS)

tran/atonev.o: tran/atonev.c tran/atonev.h $(NYQDEP)
	$(CC) -c tran/atonev.c -o tran/atonev.o $(CFLAGS)

tran/biquadfilt.o: tran/biquadfilt.c tran/biquadfilt.h $(NYQDEP)
	$(CC) -c tran/biquadfilt.c -o tran/biquadfilt.o $(CFLAGS)

tran/buzz.o: tran/buzz.c tran/buzz.h $(NYQDEP)
	$(CC) -c tran/buzz.c -o tran/buzz.o $(CFLAGS)

tran/chase.o: tran/chase.c tran/chase.h $(NYQDEP)
	$(CC) -c tran/chase.c -o tran/chase.o $(CFLAGS)

tran/clip.o: tran/clip.c tran/clip.h $(NYQDEP)
	$(CC) -c tran/clip.c -o tran/clip.o $(CFLAGS)

tran/congen.o: tran/congen.c tran/congen.h $(NYQDEP)
	$(CC) -c tran/congen.c -o tran/congen.o $(CFLAGS)

tran/const.o: tran/const.c tran/const.h $(NYQDEP)
	$(CC) -c tran/const.c -o tran/const.o $(CFLAGS)

tran/coterm.o: tran/coterm.c tran/coterm.h $(NYQDEP)
	$(CC) -c tran/coterm.c -o tran/coterm.o $(CFLAGS)

tran/delaycc.o: tran/delaycc.c tran/delaycc.h $(NYQDEP)
	$(CC) -c tran/delaycc.c -o tran/delaycc.o $(CFLAGS)

tran/delaycv.o: tran/delaycv.c tran/delaycv.h $(NYQDEP)
	$(CC) -c tran/delaycv.c -o tran/delaycv.o $(CFLAGS)

tran/eqbandvvv.o: tran/eqbandvvv.c tran/eqbandvvv.h $(NYQDEP)
	$(CC) -c tran/eqbandvvv.c -o tran/eqbandvvv.o $(CFLAGS)

tran/exp.o: tran/exp.c tran/exp.h $(NYQDEP)
	$(CC) -c tran/exp.c -o tran/exp.o $(CFLAGS)

tran/follow.o: tran/follow.c tran/follow.h $(NYQDEP)
	$(CC) -c tran/follow.c -o tran/follow.o $(CFLAGS)

tran/fmosc.o: tran/fmosc.c tran/fmosc.h $(NYQDEP)
	$(CC) -c tran/fmosc.c -o tran/fmosc.o $(CFLAGS)

tran/fromobject.o: tran/fromobject.c tran/fromobject.h $(NYQDEP)
	$(CC) -c tran/fromobject.c -o tran/fromobject.o $(CFLAGS)

tran/fromarraystream.o: tran/fromarraystream.c tran/fromarraystream.h $(NYQDEP)
	$(CC) -c tran/fromarraystream.c -o tran/fromarraystream.o $(CFLAGS)

tran/gate.o: tran/gate.c tran/gate.h $(NYQDEP)
	$(CC) -c tran/gate.c -o tran/gate.o $(CFLAGS)

tran/ifft.o: tran/ifft.c tran/ifft.h $(NYQDEP)
	$(CC) -c tran/ifft.c -o tran/ifft.o $(CFLAGS)

tran/instrclar.o: tran/instrclar.c tran/instrclar.h $(NYQDEP)
	$(CC) -c tran/instrclar.c -o tran/instrclar.o $(CFLAGS)

tran/instrclarall.o: tran/instrclarall.c tran/instrclarall.h $(NYQDEP)
	$(CC) -c tran/instrclarall.c -o tran/instrclarall.o $(CFLAGS)

tran/instrclarfreq.o: tran/instrclarfreq.c tran/instrclarfreq.h $(NYQDEP)
	$(CC) -c tran/instrclarfreq.c -o tran/instrclarfreq.o $(CFLAGS)

tran/instrsax.o: tran/instrsax.c tran/instrsax.h $(NYQDEP)
	$(CC) -c tran/instrsax.c -o tran/instrsax.o $(CFLAGS)

tran/instrsaxall.o: tran/instrsaxall.c tran/instrsaxall.h $(NYQDEP)
	$(CC) -c tran/instrsaxall.c -o tran/instrsaxall.o $(CFLAGS)

tran/instrsaxfreq.o: tran/instrsaxfreq.c tran/instrsaxfreq.h $(NYQDEP)
	$(CC) -c tran/instrsaxfreq.c -o tran/instrsaxfreq.o $(CFLAGS)

tran/integrate.o: tran/integrate.c tran/integrate.h $(NYQDEP)
	$(CC) -c tran/integrate.c -o tran/integrate.o $(CFLAGS)

tran/log.o: tran/log.c tran/log.h $(NYQDEP)
	$(CC) -c tran/log.c -o tran/log.o $(CFLAGS)

tran/lpreson.o: tran/lpreson.c tran/lpreson.h $(NYQDEP)
	$(CC) -c tran/lpreson.c -o tran/lpreson.o $(CFLAGS)

tran/maxv.o: tran/maxv.c tran/maxv.h $(NYQDEP)
	$(CC) -c tran/maxv.c -o tran/maxv.o $(CFLAGS)

tran/offset.o: tran/offset.c tran/offset.h $(NYQDEP)
	$(CC) -c tran/offset.c -o tran/offset.o $(CFLAGS)

tran/oneshot.o: tran/oneshot.c tran/oneshot.h $(NYQDEP)
	$(CC) -c tran/oneshot.c -o tran/oneshot.o $(CFLAGS)

tran/osc.o: tran/osc.c tran/osc.h $(NYQDEP)
	$(CC) -c tran/osc.c -o tran/osc.o $(CFLAGS)

tran/partial.o: tran/partial.c tran/partial.h $(NYQDEP)
	$(CC) -c tran/partial.c -o tran/partial.o $(CFLAGS)

tran/pluck.o: tran/pluck.c tran/pluck.h $(NYQDEP)
	$(CC) -c tran/pluck.c -o tran/pluck.o $(CFLAGS)

tran/prod.o: tran/prod.c tran/prod.h $(NYQDEP)
	$(CC) -c tran/prod.c -o tran/prod.o $(CFLAGS)

tran/pwl.o: tran/pwl.c tran/pwl.h $(NYQDEP)
	$(CC) -c tran/pwl.c -o tran/pwl.o $(CFLAGS)

tran/quantize.o: tran/quantize.c tran/quantize.h $(NYQDEP)
	$(CC) -c tran/quantize.c -o tran/quantize.o $(CFLAGS)

tran/recip.o: tran/recip.c tran/recip.h $(NYQDEP)
	$(CC) -c tran/recip.c -o tran/recip.o $(CFLAGS)

tran/reson.o: tran/reson.c tran/reson.h $(NYQDEP)
	$(CC) -c tran/reson.c -o tran/reson.o $(CFLAGS)

tran/resonvc.o: tran/resonvc.c tran/resonvc.h $(NYQDEP)
	$(CC) -c tran/resonvc.c -o tran/resonvc.o $(CFLAGS)

tran/resoncv.o: tran/resoncv.c tran/resoncv.h $(NYQDEP)
	$(CC) -c tran/resoncv.c -o tran/resoncv.o $(CFLAGS)

tran/resonvv.o: tran/resonvv.c tran/resonvv.h $(NYQDEP)
	$(CC) -c tran/resonvv.c -o tran/resonvv.o $(CFLAGS)

tran/sampler.o: tran/sampler.c tran/sampler.h $(NYQDEP)
	$(CC) -c tran/sampler.c -o tran/sampler.o $(CFLAGS)

tran/scale.o: tran/scale.c tran/scale.h $(NYQDEP)
	$(CC) -c tran/scale.c -o tran/scale.o $(CFLAGS)

tran/shape.o: tran/shape.c tran/shape.h $(NYQDEP)
	$(CC) -c tran/shape.c -o tran/shape.o $(CFLAGS)

tran/sine.o: tran/sine.c tran/sine.h $(NYQDEP)
	$(CC) -c tran/sine.c -o tran/sine.o $(CFLAGS)

tran/siosc.o: tran/siosc.c tran/siosc.h $(NYQDEP)
	$(CC) -c tran/siosc.c -o tran/siosc.o $(CFLAGS)

tran/slope.o: tran/slope.c tran/slope.h $(NYQDEP)
	$(CC) -c tran/slope.c -o tran/slope.o $(CFLAGS)

tran/sqrt.o: tran/sqrt.c tran/sqrt.h $(NYQDEP)
	$(CC) -c tran/sqrt.c -o tran/sqrt.o $(CFLAGS)

tran/tapf.o: tran/tapf.c tran/tapf.h $(NYQDEP)
	$(CC) -c tran/tapf.c -o tran/tapf.o $(CFLAGS)

tran/tapv.o: tran/tapv.c tran/tapv.h $(NYQDEP)
	$(CC) -c tran/tapv.c -o tran/tapv.o $(CFLAGS)

tran/tone.o: tran/tone.c tran/tone.h $(NYQDEP)
	$(CC) -c tran/tone.c -o tran/tone.o $(CFLAGS)

tran/tonev.o: tran/tonev.c tran/tonev.h $(NYQDEP)
	$(CC) -c tran/tonev.c -o tran/tonev.o $(CFLAGS)

tran/upsample.o: tran/upsample.c tran/upsample.h $(NYQDEP)
	$(CC) -c tran/upsample.c -o tran/upsample.o $(CFLAGS)

tran/white.o: tran/white.c tran/white.h $(NYQDEP)
	$(CC) -c tran/white.c -o tran/white.o $(CFLAGS)

tran/stkrev.o: tran/stkrev.c tran/stkrev.h $(NYQDEP)
	$(CC) -c tran/stkrev.c -o tran/stkrev.o $(CFLAGS)

tran/stkpitshift.o: tran/stkpitshift.c tran/stkpitshift.h $(NYQDEP)
	$(CC) -c tran/stkpitshift.c -o tran/stkpitshift.o $(CFLAGS)

tran/stkchorus.o: tran/stkchorus.c tran/stkchorus.h $(NYQDEP)
	$(CC) -c tran/stkchorus.c -o tran/stkchorus.o $(CFLAGS)

tran/instrbow.o: tran/instrbow.c tran/instrbow.h $(NYQDEP)
	$(CC) -c tran/instrbow.c -o tran/instrbow.o $(CFLAGS)

tran/instrbowedfreq.o: tran/instrbowedfreq.c tran/instrbowedfreq.h $(NYQDEP)
	$(CC) -c tran/instrbowedfreq.c -o tran/instrbowedfreq.o $(CFLAGS)

tran/instrbanded.o: tran/instrbanded.c tran/instrbanded.h $(NYQDEP)
	$(CC) -c tran/instrbanded.c -o tran/instrbanded.o $(CFLAGS)

tran/instrmandolin.o: tran/instrmandolin.c tran/instrmandolin.h $(NYQDEP)
	$(CC) -c tran/instrmandolin.c -o tran/instrmandolin.o $(CFLAGS)

tran/instrsitar.o: tran/instrsitar.c tran/instrsitar.h $(NYQDEP)
	$(CC) -c tran/instrsitar.c -o tran/instrsitar.o $(CFLAGS)

tran/instrmodalbar.o: tran/instrmodalbar.c tran/instrmodalbar.h $(NYQDEP)
	$(CC) -c tran/instrmodalbar.c -o tran/instrmodalbar.o $(CFLAGS)

tran/instrflute.o: tran/instrflute.c tran/instrflute.h $(NYQDEP)
	$(CC) -c tran/instrflute.c -o tran/instrflute.o $(CFLAGS)

tran/instrflutefreq.o: tran/instrflutefreq.c tran/instrflutefreq.h $(NYQDEP)
	$(CC) -c tran/instrflutefreq.c -o tran/instrflutefreq.o $(CFLAGS)

tran/instrfluteall.o: tran/instrfluteall.c tran/instrfluteall.h $(NYQDEP)
	$(CC) -c tran/instrfluteall.c -o tran/instrfluteall.o $(CFLAGS)

tran/fmfb.o: tran/fmfb.c tran/fmfb.h $(NYQDEP)
	$(CC) -c tran/fmfb.c -o tran/fmfb.o $(CFLAGS)

tran/fmfbv.o: tran/fmfbv.c tran/fmfbv.h $(NYQDEP)
	$(CC) -c tran/fmfbv.c -o tran/fmfbv.o $(CFLAGS)

nyqsrc/sndfnint.o: nyqsrc/sndfnint.c 
	$(CC) -c nyqsrc/sndfnint.c -o nyqsrc/sndfnint.o $(CFLAGS)

nyqsrc/seqfnint.o: nyqsrc/seqfnint.c 
	$(CC) -c nyqsrc/seqfnint.c -o nyqsrc/seqfnint.o $(CFLAGS)

nyqstk/stkinit.o: nyqstk/stkinit.cpp nyqstk/stkinit.h
	g++ -c nyqstk/stkinit.cpp -o nyqstk/stkinit.o $(CFLAGS)

nyqstk/instr.o: nyqstk/instr.cpp nyqstk/instr.h
	g++ -c nyqstk/instr.cpp -o nyqstk/instr.o $(CFLAGS)

nyqstk/stkint.o: nyqstk/stkint.cpp nyqstk/stkint.h
	g++ -c nyqstk/stkint.cpp -o nyqstk/stkint.o $(CFLAGS)

nyqstk/src/Generator.o: nyqstk/src/Generator.cpp nyqstk/include/Generator.h
	g++ -c nyqstk/src/Generator.cpp -o nyqstk/src/Generator.o $(CFLAGS)

nyqstk/src/SineWave.o: nyqstk/src/SineWave.cpp nyqstk/include/SineWave.h
	g++ -c nyqstk/src/SineWave.cpp -o nyqstk/src/SineWave.o $(CFLAGS)

nyqstk/src/Function.o: nyqstk/src/Function.cpp nyqstk/include/Function.h
	g++ -c nyqstk/src/Function.cpp -o nyqstk/src/Function.o $(CFLAGS)

nyqstk/src/FileRead.o: nyqstk/src/FileRead.cpp nyqstk/include/FileRead.h
	g++ -c nyqstk/src/FileRead.cpp -o nyqstk/src/FileRead.o $(CFLAGS)

nyqstk/src/FileWvIn.o: nyqstk/src/FileWvIn.cpp nyqstk/include/FileWvIn.h
	g++ -c nyqstk/src/FileWvIn.cpp -o nyqstk/src/FileWvIn.o $(CFLAGS)

nyqstk/src/Effect.o: nyqstk/src/Effect.cpp nyqstk/include/Effect.h
	g++ -c nyqstk/src/Effect.cpp -o nyqstk/src/Effect.o $(CFLAGS)

nyqstk/src/Clarinet.o: nyqstk/src/Clarinet.cpp nyqstk/include/Clarinet.h
	g++ -c nyqstk/src/Clarinet.cpp -o nyqstk/src/Clarinet.o $(CFLAGS)

nyqstk/src/Delay.o: nyqstk/src/Delay.cpp nyqstk/include/Delay.h
	g++ -c nyqstk/src/Delay.cpp -o nyqstk/src/Delay.o $(CFLAGS)

nyqstk/src/DelayL.o: nyqstk/src/DelayL.cpp nyqstk/include/DelayL.h
	g++ -c nyqstk/src/DelayL.cpp -o nyqstk/src/DelayL.o $(CFLAGS)

nyqstk/src/Envelope.o: nyqstk/src/Envelope.cpp nyqstk/include/Envelope.h
	g++ -c nyqstk/src/Envelope.cpp -o nyqstk/src/Envelope.o $(CFLAGS)

nyqstk/src/Filter.o: nyqstk/src/Filter.cpp nyqstk/include/Filter.h
	g++ -c nyqstk/src/Filter.cpp -o nyqstk/src/Filter.o $(CFLAGS)

nyqstk/src/Instrmnt.o: nyqstk/src/Instrmnt.cpp nyqstk/include/Instrmnt.h
	g++ -c nyqstk/src/Instrmnt.cpp -o nyqstk/src/Instrmnt.o $(CFLAGS)

nyqstk/src/Noise.o: nyqstk/src/Noise.cpp nyqstk/include/Noise.h
	g++ -c nyqstk/src/Noise.cpp -o nyqstk/src/Noise.o $(CFLAGS)

nyqstk/src/OneZero.o: nyqstk/src/OneZero.cpp nyqstk/include/OneZero.h
	g++ -c nyqstk/src/OneZero.cpp -o nyqstk/src/OneZero.o $(CFLAGS)

nyqstk/src/ReedTable.o: nyqstk/src/ReedTable.cpp nyqstk/include/ReedTable.h
	g++ -c nyqstk/src/ReedTable.cpp -o nyqstk/src/ReedTable.o $(CFLAGS)

nyqstk/src/Saxofony.o: nyqstk/src/Saxofony.cpp nyqstk/include/Saxofony.h
	g++ -c nyqstk/src/Saxofony.cpp -o nyqstk/src/Saxofony.o $(CFLAGS)

nyqstk/src/Stk.o: nyqstk/src/Stk.cpp nyqstk/include/Stk.h
	g++ -c nyqstk/src/Stk.cpp -o nyqstk/src/Stk.o $(CFLAGS)

nyqstk/src/WaveLoop.o: nyqstk/src/WaveLoop.cpp nyqstk/include/WaveLoop.h
	g++ -c nyqstk/src/WaveLoop.cpp -o nyqstk/src/WaveLoop.o $(CFLAGS)

nyqstk/src/WvIn.o: nyqstk/src/WvIn.cpp nyqstk/include/WvIn.h
	g++ -c nyqstk/src/WvIn.cpp -o nyqstk/src/WvIn.o $(CFLAGS)

nyqstk/src/NRev.o: nyqstk/src/NRev.cpp nyqstk/include/NRev.h
	g++ -c nyqstk/src/NRev.cpp -o nyqstk/src/NRev.o $(CFLAGS)

nyqstk/src/JCRev.o: nyqstk/src/JCRev.cpp nyqstk/include/JCRev.h
	g++ -c nyqstk/src/JCRev.cpp -o nyqstk/src/JCRev.o $(CFLAGS)

nyqstk/src/PRCRev.o: nyqstk/src/PRCRev.cpp nyqstk/include/PRCRev.h
	g++ -c nyqstk/src/PRCRev.cpp -o nyqstk/src/PRCRev.o $(CFLAGS)

nyqstk/src/PitShift.o: nyqstk/src/PitShift.cpp nyqstk/include/PitShift.h
	g++ -c nyqstk/src/PitShift.cpp -o nyqstk/src/PitShift.o $(CFLAGS)

nyqstk/src/Chorus.o: nyqstk/src/Chorus.cpp nyqstk/include/Chorus.h
	g++ -c nyqstk/src/Chorus.cpp -o nyqstk/src/Chorus.o $(CFLAGS)

nyqstk/src/Bowed.o: nyqstk/src/Bowed.cpp nyqstk/include/Bowed.h
	g++ -c nyqstk/src/Bowed.cpp -o nyqstk/src/Bowed.o $(CFLAGS)

nyqstk/src/BowTable.o: nyqstk/src/BowTable.cpp nyqstk/include/BowTable.h
	g++ -c nyqstk/src/BowTable.cpp -o nyqstk/src/BowTable.o $(CFLAGS)

nyqstk/src/ADSR.o: nyqstk/src/ADSR.cpp nyqstk/include/ADSR.h
	g++ -c nyqstk/src/ADSR.cpp -o nyqstk/src/ADSR.o $(CFLAGS)

nyqstk/src/OnePole.o: nyqstk/src/OnePole.cpp nyqstk/include/OnePole.h
	g++ -c nyqstk/src/OnePole.cpp -o nyqstk/src/OnePole.o $(CFLAGS)

nyqstk/src/BiQuad.o: nyqstk/src/BiQuad.cpp nyqstk/include/BiQuad.h
	g++ -c nyqstk/src/BiQuad.cpp -o nyqstk/src/BiQuad.o $(CFLAGS)

nyqstk/src/BandedWG.o: nyqstk/src/BandedWG.cpp nyqstk/include/BandedWG.h
	g++ -c nyqstk/src/BandedWG.cpp -o nyqstk/src/BandedWG.o $(CFLAGS)

nyqstk/src/DelayA.o: nyqstk/src/DelayA.cpp nyqstk/include/DelayA.h
	g++ -c nyqstk/src/DelayA.cpp -o nyqstk/src/DelayA.o $(CFLAGS)

nyqstk/src/Mandolin.o: nyqstk/src/Mandolin.cpp nyqstk/include/Mandolin.h
	g++ -c nyqstk/src/Mandolin.cpp -o nyqstk/src/Mandolin.o $(CFLAGS)

nyqstk/src/PluckTwo.o: nyqstk/src/PluckTwo.cpp nyqstk/include/PluckTwo.h
	g++ -c nyqstk/src/PluckTwo.cpp -o nyqstk/src/PluckTwo.o $(CFLAGS)

nyqstk/src/Sitar.o: nyqstk/src/Sitar.cpp nyqstk/include/Sitar.h
	g++ -c nyqstk/src/Sitar.cpp -o nyqstk/src/Sitar.o $(CFLAGS)

nyqstk/src/ModalBar.o: nyqstk/src/ModalBar.cpp nyqstk/include/ModalBar.h
	g++ -c nyqstk/src/ModalBar.cpp -o nyqstk/src/ModalBar.o $(CFLAGS)

nyqstk/src/Modal.o: nyqstk/src/Modal.cpp nyqstk/include/Modal.h
	g++ -c nyqstk/src/Modal.cpp -o nyqstk/src/Modal.o $(CFLAGS)

nyqstk/src/Flute.o: nyqstk/src/Flute.cpp nyqstk/include/Flute.h
	g++ -c nyqstk/src/Flute.cpp -o nyqstk/src/Flute.o $(CFLAGS)

nyqstk/src/JetTable.o: nyqstk/src/JetTable.cpp nyqstk/include/JetTable.h
	g++ -c nyqstk/src/JetTable.cpp -o nyqstk/src/JetTable.o $(CFLAGS)

nyqstk/src/PoleZero.o: nyqstk/src/PoleZero.cpp nyqstk/include/PoleZero.h
	g++ -c nyqstk/src/PoleZero.cpp -o nyqstk/src/PoleZero.o $(CFLAGS)

xlisp/xlftab.o: nyqsrc/sndfnintptrs.h nyqsrc/sndfnintdefs.h nyqsrc/seqfnintptrs.h nyqsrc/seqfnintdefs.h
	$(CC) -c xlisp/xlftab.c -o xlisp/xlftab.o $(CFLAGS)

xlisp/extern.o: xlisp/extern.c
	$(CC) -c xlisp/extern.c -o xlisp/extern.o $(CFLAGS)

xlisp/xldmem.o: xlisp/xldmem.c
	$(CC) -c xlisp/xldmem.c -o xlisp/xldmem.o $(CFLAGS)

xlisp/xlbfun.o: xlisp/xlbfun.c
	$(CC) -c xlisp/xlbfun.c -o xlisp/xlbfun.o $(CFLAGS)

xlisp/xlcont.o: xlisp/xlcont.c
	$(CC) -c xlisp/xlcont.c -o xlisp/xlcont.o $(CFLAGS)

xlisp/xldbug.o: xlisp/xldbug.c
	$(CC) -c xlisp/xldbug.c -o xlisp/xldbug.o $(CFLAGS)

xlisp/xleval.o: xlisp/xleval.c
	$(CC) -c xlisp/xleval.c -o xlisp/xleval.o $(CFLAGS)

xlisp/xlfio.o: xlisp/xlfio.c
	$(CC) -c xlisp/xlfio.c -o xlisp/xlfio.o $(CFLAGS)

xlisp/xlglob.o: xlisp/xlglob.c
	$(CC) -c xlisp/xlglob.c -o xlisp/xlglob.o $(CFLAGS)

xlisp/xlimage.o: xlisp/xlimage.c
	$(CC) -c xlisp/xlimage.c -o xlisp/xlimage.o $(CFLAGS)

xlisp/xlinit.o: xlisp/xlinit.c
	$(CC) -c xlisp/xlinit.c -o xlisp/xlinit.o $(CFLAGS)

xlisp/xlio.o: xlisp/xlio.c
	$(CC) -c xlisp/xlio.c -o xlisp/xlio.o $(CFLAGS)

xlisp/xlisp.o: xlisp/xlisp.c
	$(CC) -c xlisp/xlisp.c -o xlisp/xlisp.o $(CFLAGS)

xlisp/xljump.o: xlisp/xljump.c
	$(CC) -c xlisp/xljump.c -o xlisp/xljump.o $(CFLAGS)

xlisp/xllist.o: xlisp/xllist.c
	$(CC) -c xlisp/xllist.c -o xlisp/xllist.o $(CFLAGS)

xlisp/xlmath.o: xlisp/xlmath.c
	$(CC) -c xlisp/xlmath.c -o xlisp/xlmath.o $(CFLAGS)

xlisp/xlobj.o: xlisp/xlobj.c
	$(CC) -c xlisp/xlobj.c -o xlisp/xlobj.o $(CFLAGS)

xlisp/xlpp.o: xlisp/xlpp.c
	$(CC) -c xlisp/xlpp.c -o xlisp/xlpp.o $(CFLAGS)

xlisp/xlprin.o: xlisp/xlprin.c
	$(CC) -c xlisp/xlprin.c -o xlisp/xlprin.o $(CFLAGS)

xlisp/xlread.o: xlisp/xlread.c
	$(CC) -c xlisp/xlread.c -o xlisp/xlread.o $(CFLAGS)

xlisp/xlstr.o: xlisp/xlstr.c
	$(CC) -c xlisp/xlstr.c -o xlisp/xlstr.o $(CFLAGS)

xlisp/xlsubr.o: xlisp/xlsubr.c
	$(CC) -c xlisp/xlsubr.c -o xlisp/xlsubr.o $(CFLAGS)

xlisp/xlsym.o: xlisp/xlsym.c
	$(CC) -c xlisp/xlsym.c -o xlisp/xlsym.o $(CFLAGS)

xlisp/xlsys.o: xlisp/xlsys.c
	$(CC) -c xlisp/xlsys.c -o xlisp/xlsys.o $(CFLAGS)

xlisp/path.o: xlisp/path.c
	$(CC) -c xlisp/path.c -o xlisp/path.o $(CFLAGS)

cmt/cext.o: cmt/cext.c
	$(CC) -c cmt/cext.c -o cmt/cext.o $(CFLAGS)

cmt/cleanup.o: cmt/cleanup.c
	$(CC) -c cmt/cleanup.c -o cmt/cleanup.o $(CFLAGS)

cmt/cmdline.o: cmt/cmdline.c
	$(CC) -c cmt/cmdline.c -o cmt/cmdline.o $(CFLAGS)

cmt/cmtcmd.o: cmt/cmtcmd.c
	$(CC) -c cmt/cmtcmd.c -o cmt/cmtcmd.o $(CFLAGS)

cmt/moxc.o: cmt/moxc.c
	$(CC) -c cmt/moxc.c -o cmt/moxc.o $(CFLAGS)

cmt/mem.o: cmt/mem.c
	$(CC) -c cmt/mem.c -o cmt/mem.o $(CFLAGS)

cmt/midifile.o: cmt/midifile.c
	$(CC) -c cmt/midifile.c -o cmt/midifile.o $(CFLAGS)

cmt/midifns.o: cmt/midifns.c
	$(CC) -c cmt/midifns.c -o cmt/midifns.o $(CFLAGS)

cmt/record.o: cmt/record.c
	$(CC) -c cmt/record.c -o cmt/record.o $(CFLAGS)

cmt/seq.o: cmt/seq.c
	$(CC) -c cmt/seq.c -o cmt/seq.o $(CFLAGS)

cmt/seqmread.o: cmt/seqmread.c
	$(CC) -c cmt/seqmread.c -o cmt/seqmread.o $(CFLAGS)

cmt/seqmwrite.o: cmt/seqmwrite.c
	$(CC) -c cmt/seqmwrite.c -o cmt/seqmwrite.o $(CFLAGS)

cmt/seqread.o: cmt/seqread.c
	$(CC) -c cmt/seqread.c -o cmt/seqread.o $(CFLAGS)

cmt/seqwrite.o: cmt/seqwrite.c
	$(CC) -c cmt/seqwrite.c -o cmt/seqwrite.o $(CFLAGS)

cmt/tempomap.o: cmt/tempomap.c
	$(CC) -c cmt/tempomap.c -o cmt/tempomap.o $(CFLAGS)

cmt/timebase.o: cmt/timebase.c
	$(CC) -c cmt/timebase.c -o cmt/timebase.o $(CFLAGS)

cmt/userio.o: cmt/userio.c
	$(CC) -c cmt/userio.c -o cmt/userio.o $(CFLAGS)

nylsf/aiff.o: nylsf/aiff.c
	$(CC) -c nylsf/aiff.c -o nylsf/aiff.o $(CFLAGS)

nylsf/alaw.o: nylsf/alaw.c
	$(CC) -c nylsf/alaw.c -o nylsf/alaw.o $(CFLAGS)

nylsf/au.o: nylsf/au.c
	$(CC) -c nylsf/au.c -o nylsf/au.o $(CFLAGS)

nylsf/audio_detect.o: nylsf/audio_detect.c
	$(CC) -c nylsf/audio_detect.c -o nylsf/audio_detect.o $(CFLAGS)

nylsf/avr.o: nylsf/avr.c
	$(CC) -c nylsf/avr.c -o nylsf/avr.o $(CFLAGS)

nylsf/broadcast.o: nylsf/broadcast.c
	$(CC) -c nylsf/broadcast.c -o nylsf/broadcast.o $(CFLAGS)

nylsf/caf.o: nylsf/caf.c
	$(CC) -c nylsf/caf.c -o nylsf/caf.o $(CFLAGS)

nylsf/chanmap.o: nylsf/chanmap.c
	$(CC) -c nylsf/chanmap.c -o nylsf/chanmap.o $(CFLAGS)

nylsf/chunk.o: nylsf/chunk.c
	$(CC) -c nylsf/chunk.c -o nylsf/chunk.o $(CFLAGS)

nylsf/command.o: nylsf/command.c
	$(CC) -c nylsf/command.c -o nylsf/command.o $(CFLAGS)

nylsf/common.o: nylsf/common.c
	$(CC) -c nylsf/common.c -o nylsf/common.o $(CFLAGS)

nylsf/dither.o: nylsf/dither.c
	$(CC) -c nylsf/dither.c -o nylsf/dither.o $(CFLAGS)

nylsf/double64.o: nylsf/double64.c
	$(CC) -c nylsf/double64.c -o nylsf/double64.o $(CFLAGS)

nylsf/dwd.o: nylsf/dwd.c
	$(CC) -c nylsf/dwd.c -o nylsf/dwd.o $(CFLAGS)

nylsf/dwvw.o: nylsf/dwvw.c
	$(CC) -c nylsf/dwvw.c -o nylsf/dwvw.o $(CFLAGS)

nylsf/file_io.o: nylsf/file_io.c
	$(CC) -c nylsf/file_io.c -o nylsf/file_io.o $(CFLAGS)

nylsf/flac.o: nylsf/flac.c
	$(CC) -c nylsf/flac.c -o nylsf/flac.o $(CFLAGS)

nylsf/float32.o: nylsf/float32.c
	$(CC) -c nylsf/float32.c -o nylsf/float32.o $(CFLAGS)

nylsf/gsm610.o: nylsf/gsm610.c
	$(CC) -c nylsf/gsm610.c -o nylsf/gsm610.o $(CFLAGS)

nylsf/htk.o: nylsf/htk.c
	$(CC) -c nylsf/htk.c -o nylsf/htk.o $(CFLAGS)

nylsf/id3.o: nylsf/id3.c
	$(CC) -c nylsf/id3.c -o nylsf/id3.o $(CFLAGS)

nylsf/ima_adpcm.o: nylsf/ima_adpcm.c
	$(CC) -c nylsf/ima_adpcm.c -o nylsf/ima_adpcm.o $(CFLAGS)

nylsf/ima_oki_adpcm.o: nylsf/ima_oki_adpcm.c
	$(CC) -c nylsf/ima_oki_adpcm.c -o nylsf/ima_oki_adpcm.o $(CFLAGS)

nylsf/interleave.o: nylsf/interleave.c
	$(CC) -c nylsf/interleave.c -o nylsf/interleave.o $(CFLAGS)

nylsf/ircam.o: nylsf/ircam.c
	$(CC) -c nylsf/ircam.c -o nylsf/ircam.o $(CFLAGS)

nylsf/macbinary3.o: nylsf/macbinary3.c
	$(CC) -c nylsf/macbinary3.c -o nylsf/macbinary3.o $(CFLAGS)

nylsf/macos.o: nylsf/macos.c
	$(CC) -c nylsf/macos.c -o nylsf/macos.o $(CFLAGS)

nylsf/mat4.o: nylsf/mat4.c
	$(CC) -c nylsf/mat4.c -o nylsf/mat4.o $(CFLAGS)

nylsf/mat5.o: nylsf/mat5.c
	$(CC) -c nylsf/mat5.c -o nylsf/mat5.o $(CFLAGS)

nylsf/mpc2k.o: nylsf/mpc2k.c
	$(CC) -c nylsf/mpc2k.c -o nylsf/mpc2k.o $(CFLAGS)

nylsf/ms_adpcm.o: nylsf/ms_adpcm.c
	$(CC) -c nylsf/ms_adpcm.c -o nylsf/ms_adpcm.o $(CFLAGS)

nylsf/nist.o: nylsf/nist.c
	$(CC) -c nylsf/nist.c -o nylsf/nist.o $(CFLAGS)

nylsf/ogg.o: nylsf/ogg.c
	$(CC) -c nylsf/ogg.c -o nylsf/ogg.o $(CFLAGS)

nylsf/ogg_vorbis.o: nylsf/ogg_vorbis.c
	$(CC) -c nylsf/ogg_vorbis.c -o nylsf/ogg_vorbis.o $(CFLAGS)

nylsf/paf.o: nylsf/paf.c
	$(CC) -c nylsf/paf.c -o nylsf/paf.o $(CFLAGS)

nylsf/pcm.o: nylsf/pcm.c
	$(CC) -c nylsf/pcm.c -o nylsf/pcm.o $(CFLAGS)

nylsf/pvf.o: nylsf/pvf.c
	$(CC) -c nylsf/pvf.c -o nylsf/pvf.o $(CFLAGS)

nylsf/raw.o: nylsf/raw.c
	$(CC) -c nylsf/raw.c -o nylsf/raw.o $(CFLAGS)

nylsf/rf64.o: nylsf/rf64.c
	$(CC) -c nylsf/rf64.c -o nylsf/rf64.o $(CFLAGS)

nylsf/rx2.o: nylsf/rx2.c
	$(CC) -c nylsf/rx2.c -o nylsf/rx2.o $(CFLAGS)

nylsf/sd2.o: nylsf/sd2.c
	$(CC) -c nylsf/sd2.c -o nylsf/sd2.o $(CFLAGS)

nylsf/sds.o: nylsf/sds.c
	$(CC) -c nylsf/sds.c -o nylsf/sds.o $(CFLAGS)

nylsf/sndfile.o: nylsf/sndfile.c
	$(CC) -c nylsf/sndfile.c -o nylsf/sndfile.o $(CFLAGS)

nylsf/strings.o: nylsf/strings.c
	$(CC) -c nylsf/strings.c -o nylsf/strings.o $(CFLAGS)

nylsf/svx.o: nylsf/svx.c
	$(CC) -c nylsf/svx.c -o nylsf/svx.o $(CFLAGS)

nylsf/txw.o: nylsf/txw.c
	$(CC) -c nylsf/txw.c -o nylsf/txw.o $(CFLAGS)

nylsf/ulaw.o: nylsf/ulaw.c
	$(CC) -c nylsf/ulaw.c -o nylsf/ulaw.o $(CFLAGS)

nylsf/voc.o: nylsf/voc.c
	$(CC) -c nylsf/voc.c -o nylsf/voc.o $(CFLAGS)

nylsf/vox_adpcm.o: nylsf/vox_adpcm.c
	$(CC) -c nylsf/vox_adpcm.c -o nylsf/vox_adpcm.o $(CFLAGS)

nylsf/w64.o: nylsf/w64.c
	$(CC) -c nylsf/w64.c -o nylsf/w64.o $(CFLAGS)

nylsf/wav.o: nylsf/wav.c
	$(CC) -c nylsf/wav.c -o nylsf/wav.o $(CFLAGS)

nylsf/wav_w64.o: nylsf/wav_w64.c
	$(CC) -c nylsf/wav_w64.c -o nylsf/wav_w64.o $(CFLAGS)

nylsf/wve.o: nylsf/wve.c
	$(CC) -c nylsf/wve.c -o nylsf/wve.o $(CFLAGS)

nylsf/xi.o: nylsf/xi.c
	$(CC) -c nylsf/xi.c -o nylsf/xi.o $(CFLAGS)

nylsf/g72x.o: nylsf/g72x.c
	$(CC) -c nylsf/g72x.c -o nylsf/g72x.o $(CFLAGS)

nylsf/GSM610/add.o: nylsf/GSM610/add.c
	$(CC) -c nylsf/GSM610/add.c -o nylsf/GSM610/add.o $(CFLAGS)

nylsf/GSM610/code.o: nylsf/GSM610/code.c
	$(CC) -c nylsf/GSM610/code.c -o nylsf/GSM610/code.o $(CFLAGS)

nylsf/GSM610/decode.o: nylsf/GSM610/decode.c
	$(CC) -c nylsf/GSM610/decode.c -o nylsf/GSM610/decode.o $(CFLAGS)

nylsf/GSM610/gsm_create.o: nylsf/GSM610/gsm_create.c
	$(CC) -c nylsf/GSM610/gsm_create.c -o nylsf/GSM610/gsm_create.o $(CFLAGS)

nylsf/GSM610/gsm_decode.o: nylsf/GSM610/gsm_decode.c
	$(CC) -c nylsf/GSM610/gsm_decode.c -o nylsf/GSM610/gsm_decode.o $(CFLAGS)

nylsf/GSM610/gsm_destroy.o: nylsf/GSM610/gsm_destroy.c
	$(CC) -c nylsf/GSM610/gsm_destroy.c -o nylsf/GSM610/gsm_destroy.o $(CFLAGS)

nylsf/GSM610/gsm_encode.o: nylsf/GSM610/gsm_encode.c
	$(CC) -c nylsf/GSM610/gsm_encode.c -o nylsf/GSM610/gsm_encode.o $(CFLAGS)

nylsf/GSM610/gsm_option.o: nylsf/GSM610/gsm_option.c
	$(CC) -c nylsf/GSM610/gsm_option.c -o nylsf/GSM610/gsm_option.o $(CFLAGS)

nylsf/GSM610/long_term.o: nylsf/GSM610/long_term.c
	$(CC) -c nylsf/GSM610/long_term.c -o nylsf/GSM610/long_term.o $(CFLAGS)

nylsf/GSM610/lpc.o: nylsf/GSM610/lpc.c
	$(CC) -c nylsf/GSM610/lpc.c -o nylsf/GSM610/lpc.o $(CFLAGS)

nylsf/GSM610/preprocess.o: nylsf/GSM610/preprocess.c
	$(CC) -c nylsf/GSM610/preprocess.c -o nylsf/GSM610/preprocess.o $(CFLAGS)

nylsf/GSM610/rpe.o: nylsf/GSM610/rpe.c
	$(CC) -c nylsf/GSM610/rpe.c -o nylsf/GSM610/rpe.o $(CFLAGS)

nylsf/GSM610/short_term.o: nylsf/GSM610/short_term.c
	$(CC) -c nylsf/GSM610/short_term.c -o nylsf/GSM610/short_term.o $(CFLAGS)

nylsf/GSM610/table.o: nylsf/GSM610/table.c
	$(CC) -c nylsf/GSM610/table.c -o nylsf/GSM610/table.o $(CFLAGS)

nylsf/G72x/g721.o: nylsf/G72x/g721.c
	$(CC) -c nylsf/G72x/g721.c -o nylsf/G72x/g721.o $(CFLAGS)

nylsf/G72x/g723_16.o: nylsf/G72x/g723_16.c
	$(CC) -c nylsf/G72x/g723_16.c -o nylsf/G72x/g723_16.o $(CFLAGS)

nylsf/G72x/g723_24.o: nylsf/G72x/g723_24.c
	$(CC) -c nylsf/G72x/g723_24.c -o nylsf/G72x/g723_24.o $(CFLAGS)

nylsf/G72x/g723_40.o: nylsf/G72x/g723_40.c
	$(CC) -c nylsf/G72x/g723_40.c -o nylsf/G72x/g723_40.o $(CFLAGS)

nylsf/G72x/g72x.o: nylsf/G72x/g72x.c
	$(CC) -c nylsf/G72x/g72x.c -o nylsf/G72x/g72x.o $(CFLAGS)

sys/unix/osstuff.o: sys/unix/osstuff.c
	$(CC) -c sys/unix/osstuff.c -o sys/unix/osstuff.o $(CFLAGS)

misc/intgen: misc/intgen.c
	cd misc; make intgen

nyqsrc/sndfnintptrs.h: $(SNDFNINT_HDRS) misc/intgen Makefile
	$(INTGEN) nyqsrc/sndfnint $(SNDFNINT_HDRS)

nyqsrc/seqfnintptrs.h: $(SEQFNINT_HDRS) misc/intgen Makefile
	$(INTGEN) nyqsrc/seqfnint $(SEQFNINT_HDRS)

clean:
	cd misc; make clean
	cd liblo; test -f Makefile && make clean || true
	cd portaudio; test -f Makefile && make clean || true
	rm -f $(OBJECTS)
# These could be deleted, but they're part of the release, so we won't
# Note that these files are machine-generated:
# 	rm -f nyqsrc/sndfnintptrs.h nyqsrc/sndfnint.c nyqsrc/sndfnintdefs.h
# 	rm -f nyqsrc/seqfnintptrs.h nyqsrc/seqfnint.c nyqsrc/seqfnintdefs.h

cleaner: clean
	cd misc; make cleaner
	rm -f *.backup */*.backup
	rm -f *~ */*.*~
	rm -f #*# */#*#
	rm -f *.save */*.save
	rm -f *.CKP */*.CKP
	rm -f *.BAK */*.BAK
	rm -f *.old */*.old
	rm -f *.gold */*.gold
	rm -f playparms
	rm -f points.dat
	rm -f core.* core
	rm -f $(NY)
