#
# Makefile for Nyquist, SYSTEM-TYPE is RS6K
# run make in the top-level Nyquist directory to compile Nyquist
#
# NOTE: this file is machine-generated.  DO NOT EDIT!
#   Instead, modify makefile.lsp and regenerate the makefile.
#   Ports and bug fixes are welcome - please mail them to 
#   dannenberg@cs.cmu.edu.  Thanks.
#

# This is the resulting executable (normally "ny"):
NY = ny

# Standard list of includes (common to all unix versions)
INCL = -Inyqsrc -Itran -Ixlisp -Isys/unix -Icmt -Isnd -Ifft

# system dependent stuff for rs6k:

MIDI = /afs/cs/project/music/rs6k/midilib
CC = cc
# change -g to -O for optimization
CFLAGS = -DCMTSTUFF -g $(INCL) -I$(MIDI)
XFLAGS = $(CFLAGS) -qlanglvl=extended
LN = xlc -qattr -qlist
LFLAGS = -lm -L$(MIDI) -lmidi -lbsd -lg


INTGEN = misc/intgen

# Object files for Nyquist:
OBJECTS = xlisp/extern.o xlisp/xldmem.o xlisp/xlbfun.o xlisp/xlcont.o  \
	xlisp/xldbug.o xlisp/xleval.o xlisp/xlfio.o xlisp/xlftab.o  \
	xlisp/xlglob.o xlisp/xlimage.o xlisp/xlinit.o xlisp/xlio.o  \
	xlisp/xlisp.o xlisp/xljump.o xlisp/xllist.o xlisp/xlmath.o  \
	xlisp/xlobj.o xlisp/xlpp.o xlisp/xlprin.o xlisp/xlread.o  \
	xlisp/xlstr.o xlisp/xlsubr.o xlisp/xlsym.o xlisp/xlsys.o  \
	tran/amosc.o tran/clip.o tran/const.o tran/fmosc.o  \
	tran/integrate.o tran/log.o tran/maxv.o tran/osc.o  \
	tran/prod.o tran/buzz.o tran/pwl.o tran/recip.o  \
	tran/upsample.o tran/scale.o tran/sine.o tran/partial.o  \
	tran/white.o tran/tone.o tran/tonev.o tran/atonev.o  \
	tran/atone.o tran/reson.o tran/areson.o tran/resonvc.o  \
	tran/resoncv.o tran/aresonvc.o tran/aresoncv.o tran/resonvv.o  \
	tran/aresonvv.o tran/offset.o tran/slope.o tran/delay.o  \
	tran/delaycv.o tran/shape.o tran/sampler.o tran/exp.o  \
	tran/siosc.o tran/follow.o tran/gate.o tran/quantize.o  \
	tran/ifft.o tran/congen.o tran/fromobject.o tran/fromarraystream.o  \
	tran/coterm.o tran/convolve.o tran/alpass.o tran/oneshot.o  \
	tran/chase.o tran/tapv.o tran/biquad.o tran/pluck.o  \
	cmt/cext.o cmt/cleanup.o cmt/cmdline.o cmt/cmtcmd.o  \
	cmt/moxc.o cmt/mem.o cmt/midifile.o cmt/midifns.o  \
	cmt/record.o cmt/seq.o cmt/seqmread.o cmt/seqmwrite.o  \
	cmt/seqread.o cmt/seqwrite.o cmt/tempomap.o cmt/timebase.o  \
	cmt/userio.o nyqsrc/debug.o nyqsrc/falloc.o nyqsrc/add.o  \
	nyqsrc/local.o nyqsrc/downsample.o nyqsrc/handlers.o nyqsrc/multiread.o  \
	nyqsrc/multiseq.o nyqsrc/samples.o nyqsrc/seqext.o nyqsrc/seqinterf.o  \
	nyqsrc/sndread.o nyqsrc/sndseq.o nyqsrc/sndwrite.o nyqsrc/sndmax.o  \
	nyqsrc/sound.o nyqsrc/stats.o nyqsrc/compose.o nyqsrc/inverse.o  \
	nyqsrc/resamp.o nyqsrc/resampv.o nyqsrc/ffilterkit.o nyqsrc/avg.o  \
	nyqsrc/fft.o nyqsrc/sndfail.o fft/fftn.o nyqsrc/sndfnint.o  \
	nyqsrc/seqfnint.o snd/audiors6k.o snd/sndrs6k.o snd/ieeecvt.o  \
	snd/snd.o snd/sndcvt.o snd/sndio.o snd/sndheader.o  \
	sys/unix/osstuff.o sys/unix/term.o 

# Sound functions to add to xlisp
NYQHDRS = snd/snd.h nyqsrc/sound.h nyqsrc/downsample.h nyqsrc/sndread.h  \
	nyqsrc/sndseq.h nyqsrc/add.h nyqsrc/multiseq.h nyqsrc/samples.h  \
	nyqsrc/sndwrite.h nyqsrc/sndmax.h nyqsrc/compose.h nyqsrc/inverse.h  \
	nyqsrc/resamp.h nyqsrc/resampv.h nyqsrc/fft.h nyqsrc/avg.h  \
	tran/amosc.h tran/clip.h tran/const.h tran/fmosc.h  \
	tran/integrate.h tran/log.h tran/maxv.h tran/osc.h  \
	tran/prod.h tran/buzz.h tran/pwl.h tran/recip.h  \
	tran/upsample.h tran/scale.h tran/sine.h tran/partial.h  \
	tran/white.h tran/tone.h tran/tonev.h tran/atonev.h  \
	tran/atone.h tran/reson.h tran/areson.h tran/resonvc.h  \
	tran/resoncv.h tran/aresonvc.h tran/aresoncv.h tran/resonvv.h  \
	tran/aresonvv.h tran/offset.h tran/slope.h tran/delaycc.h  \
	tran/delaycv.h tran/shape.h tran/sampler.h tran/exp.h  \
	tran/siosc.h tran/follow.h tran/gate.h tran/quantize.h  \
	tran/ifft.h tran/congen.h tran/fromobject.h tran/fromarraystream.h  \
	tran/coterm.h tran/convolve.h tran/alpass.h tran/oneshot.h  \
	tran/chase.h tran/tapv.h tran/biquad.h tran/pluck.h 

CMTHDRS = cmt/seqdecls.h nyqsrc/seqext.h cmt/seq.h nyqsrc/seqinterf.h  \
	cmt/seqread.h cmt/seqmread.h cmt/seqwrite.h cmt/seqmwrite.h 


EVERYTHING = $(NY) runtime/system.lsp

CURRENT = $(EVERYTHING)

current: $(CURRENT)

$(NY): $(OBJECTS)
	$(LN) $(OBJECTS) $(LFLAGS) -o $(NY)

# copy appropriate system.lsp and make it read-only;
# changes should be made to sys/unix/<system>/system.lsp
runtime/system.lsp: sys/unix/rs6k/system.lsp
	chmod +w runtime/system.lsp
	cp -p sys/unix/rs6k/system.lsp runtime/system.lsp
	chmod -w runtime/system.lsp

nyqsrc/debug.o: nyqsrc/debug.c nyqsrc/debug.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/debug.c -o nyqsrc/debug.o $(CFLAGS)

nyqsrc/falloc.o: nyqsrc/falloc.c nyqsrc/falloc.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/falloc.c -o nyqsrc/falloc.o $(CFLAGS)

nyqsrc/add.o: nyqsrc/add.c nyqsrc/add.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/add.c -o nyqsrc/add.o $(CFLAGS)

nyqsrc/local.o: nyqsrc/local.c xlisp/xlisp.h nyqsrc/sound.h
	cc -c nyqsrc/local.c -o nyqsrc/local.o $(CFLAGS)

nyqsrc/downsample.o: nyqsrc/downsample.c nyqsrc/downsample.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/downsample.c -o nyqsrc/downsample.o $(CFLAGS)

nyqsrc/handlers.o: nyqsrc/handlers.c 
	cc -c nyqsrc/handlers.c -o nyqsrc/handlers.o $(CFLAGS)

nyqsrc/multiread.o: nyqsrc/multiread.c nyqsrc/multiread.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/multiread.c -o nyqsrc/multiread.o $(CFLAGS)

nyqsrc/multiseq.o: nyqsrc/multiseq.c nyqsrc/multiseq.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/multiseq.c -o nyqsrc/multiseq.o $(CFLAGS)

nyqsrc/samples.o: nyqsrc/samples.c nyqsrc/samples.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/samples.c -o nyqsrc/samples.o $(CFLAGS)

nyqsrc/seqext.o: nyqsrc/seqext.c nyqsrc/seqext.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/seqext.c -o nyqsrc/seqext.o $(CFLAGS)

nyqsrc/seqinterf.o: nyqsrc/seqinterf.c nyqsrc/seqinterf.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/seqinterf.c -o nyqsrc/seqinterf.o $(CFLAGS)

nyqsrc/sndread.o: nyqsrc/sndread.c nyqsrc/sndread.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/sndread.c -o nyqsrc/sndread.o $(CFLAGS)

nyqsrc/sndseq.o: nyqsrc/sndseq.c nyqsrc/sndseq.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/sndseq.c -o nyqsrc/sndseq.o $(CFLAGS)

nyqsrc/sndwrite.o: nyqsrc/sndwrite.c nyqsrc/sndwrite.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/sndwrite.c -o nyqsrc/sndwrite.o $(CFLAGS)

nyqsrc/sndmax.o: nyqsrc/sndmax.c nyqsrc/sndmax.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/sndmax.c -o nyqsrc/sndmax.o $(CFLAGS)

nyqsrc/sound.o: nyqsrc/sound.c nyqsrc/sound.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/sound.c -o nyqsrc/sound.o $(CFLAGS)

nyqsrc/stats.o: nyqsrc/stats.c nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/stats.c -o nyqsrc/stats.o $(CFLAGS)

nyqsrc/compose.o: nyqsrc/compose.c nyqsrc/compose.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/compose.c -o nyqsrc/compose.o $(CFLAGS)

nyqsrc/inverse.o: nyqsrc/inverse.c nyqsrc/inverse.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/inverse.c -o nyqsrc/inverse.o $(CFLAGS)

nyqsrc/resamp.o: nyqsrc/resamp.c nyqsrc/resamp.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/resamp.c -o nyqsrc/resamp.o $(CFLAGS)

nyqsrc/resampv.o: nyqsrc/resampv.c nyqsrc/resampv.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/resampv.c -o nyqsrc/resampv.o $(CFLAGS)

nyqsrc/ffilterkit.o: nyqsrc/ffilterkit.c nyqsrc/ffilterkit.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/ffilterkit.c -o nyqsrc/ffilterkit.o $(CFLAGS)

nyqsrc/avg.o: nyqsrc/avg.c nyqsrc/avg.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/avg.c -o nyqsrc/avg.o $(CFLAGS)

nyqsrc/fft.o: nyqsrc/fft.c nyqsrc/fft.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c nyqsrc/fft.c -o nyqsrc/fft.o $(CFLAGS)

nyqsrc/sndfail.o: nyqsrc/sndfail.c 
	cc -c nyqsrc/sndfail.c -o nyqsrc/sndfail.o $(CFLAGS)

snd/audiors6k.o: snd/audiors6k.c snd/snd.h
	cc -c snd/audiors6k.c -o snd/audiors6k.o $(CFLAGS)

snd/sndrs6k.o: snd/sndrs6k.c snd/sndrs6k.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c snd/sndrs6k.c -o snd/sndrs6k.o $(CFLAGS)

snd/ieeecvt.o: snd/ieeecvt.c snd/ieeecvt.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c snd/ieeecvt.c -o snd/ieeecvt.o $(CFLAGS)

snd/snd.o: snd/snd.c snd/snd.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c snd/snd.c -o snd/snd.o $(CFLAGS)

snd/sndcvt.o: snd/sndcvt.c snd/snd.h
	cc -c snd/sndcvt.c -o snd/sndcvt.o $(CFLAGS)

snd/sndio.o: snd/sndio.c snd/snd.h
	cc -c snd/sndio.c -o snd/sndio.o $(CFLAGS)

snd/sndheader.o: snd/sndheader.c snd/sndheader.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c snd/sndheader.c -o snd/sndheader.o $(CFLAGS)

fft/fftn.o: fft/fftn.c fft/fftn.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c fft/fftn.c -o fft/fftn.o $(CFLAGS)

tran/amosc.o: tran/amosc.c tran/amosc.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/amosc.c -o tran/amosc.o $(CFLAGS)

tran/clip.o: tran/clip.c tran/clip.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/clip.c -o tran/clip.o $(CFLAGS)

tran/const.o: tran/const.c tran/const.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/const.c -o tran/const.o $(CFLAGS)

tran/fmosc.o: tran/fmosc.c tran/fmosc.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/fmosc.c -o tran/fmosc.o $(CFLAGS)

tran/integrate.o: tran/integrate.c tran/integrate.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/integrate.c -o tran/integrate.o $(CFLAGS)

tran/log.o: tran/log.c tran/log.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/log.c -o tran/log.o $(CFLAGS)

tran/maxv.o: tran/maxv.c tran/maxv.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/maxv.c -o tran/maxv.o $(CFLAGS)

tran/osc.o: tran/osc.c tran/osc.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/osc.c -o tran/osc.o $(CFLAGS)

tran/prod.o: tran/prod.c tran/prod.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/prod.c -o tran/prod.o $(CFLAGS)

tran/buzz.o: tran/buzz.c tran/buzz.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/buzz.c -o tran/buzz.o $(CFLAGS)

tran/pwl.o: tran/pwl.c tran/pwl.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/pwl.c -o tran/pwl.o $(CFLAGS)

tran/recip.o: tran/recip.c tran/recip.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/recip.c -o tran/recip.o $(CFLAGS)

tran/upsample.o: tran/upsample.c tran/upsample.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/upsample.c -o tran/upsample.o $(CFLAGS)

tran/scale.o: tran/scale.c tran/scale.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/scale.c -o tran/scale.o $(CFLAGS)

tran/sine.o: tran/sine.c tran/sine.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/sine.c -o tran/sine.o $(CFLAGS)

tran/partial.o: tran/partial.c tran/partial.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/partial.c -o tran/partial.o $(CFLAGS)

tran/white.o: tran/white.c tran/white.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/white.c -o tran/white.o $(CFLAGS)

tran/tone.o: tran/tone.c tran/tone.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/tone.c -o tran/tone.o $(CFLAGS)

tran/tonev.o: tran/tonev.c tran/tonev.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/tonev.c -o tran/tonev.o $(CFLAGS)

tran/atonev.o: tran/atonev.c tran/atonev.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/atonev.c -o tran/atonev.o $(CFLAGS)

tran/atone.o: tran/atone.c tran/atone.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/atone.c -o tran/atone.o $(CFLAGS)

tran/reson.o: tran/reson.c tran/reson.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/reson.c -o tran/reson.o $(CFLAGS)

tran/areson.o: tran/areson.c tran/areson.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/areson.c -o tran/areson.o $(CFLAGS)

tran/resonvc.o: tran/resonvc.c tran/resonvc.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/resonvc.c -o tran/resonvc.o $(CFLAGS)

tran/resoncv.o: tran/resoncv.c tran/resoncv.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/resoncv.c -o tran/resoncv.o $(CFLAGS)

tran/aresonvc.o: tran/aresonvc.c tran/aresonvc.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/aresonvc.c -o tran/aresonvc.o $(CFLAGS)

tran/aresoncv.o: tran/aresoncv.c tran/aresoncv.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/aresoncv.c -o tran/aresoncv.o $(CFLAGS)

tran/resonvv.o: tran/resonvv.c tran/resonvv.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/resonvv.c -o tran/resonvv.o $(CFLAGS)

tran/aresonvv.o: tran/aresonvv.c tran/aresonvv.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/aresonvv.c -o tran/aresonvv.o $(CFLAGS)

tran/offset.o: tran/offset.c tran/offset.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/offset.c -o tran/offset.o $(CFLAGS)

tran/slope.o: tran/slope.c tran/slope.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/slope.c -o tran/slope.o $(CFLAGS)

tran/delaycc.o: tran/delaycc.c tran/delaycc.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/delaycc.c -o tran/delaycc.o $(CFLAGS)

tran/delaycv.o: tran/delaycv.c tran/delaycv.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/delaycv.c -o tran/delaycv.o $(CFLAGS)

tran/shape.o: tran/shape.c tran/shape.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/shape.c -o tran/shape.o $(CFLAGS)

tran/sampler.o: tran/sampler.c tran/sampler.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/sampler.c -o tran/sampler.o $(CFLAGS)

tran/exp.o: tran/exp.c tran/exp.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/exp.c -o tran/exp.o $(CFLAGS)

tran/siosc.o: tran/siosc.c tran/siosc.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/siosc.c -o tran/siosc.o $(CFLAGS)

tran/follow.o: tran/follow.c tran/follow.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/follow.c -o tran/follow.o $(CFLAGS)

tran/gate.o: tran/gate.c tran/gate.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/gate.c -o tran/gate.o $(CFLAGS)

tran/quantize.o: tran/quantize.c tran/quantize.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/quantize.c -o tran/quantize.o $(CFLAGS)

tran/ifft.o: tran/ifft.c tran/ifft.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/ifft.c -o tran/ifft.o $(CFLAGS)

tran/congen.o: tran/congen.c tran/congen.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/congen.c -o tran/congen.o $(CFLAGS)

tran/fromobject.o: tran/fromobject.c tran/fromobject.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/fromobject.c -o tran/fromobject.o $(CFLAGS)

tran/fromarraystream.o: tran/fromarraystream.c tran/fromarraystream.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/fromarraystream.c -o tran/fromarraystream.o $(CFLAGS)

tran/coterm.o: tran/coterm.c tran/coterm.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/coterm.c -o tran/coterm.o $(CFLAGS)

tran/convolve.o: tran/convolve.c tran/convolve.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/convolve.c -o tran/convolve.o $(CFLAGS)

tran/alpass.o: tran/alpass.c tran/alpass.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/alpass.c -o tran/alpass.o $(CFLAGS)

tran/oneshot.o: tran/oneshot.c tran/oneshot.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/oneshot.c -o tran/oneshot.o $(CFLAGS)

tran/chase.o: tran/chase.c tran/chase.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/chase.c -o tran/chase.o $(CFLAGS)

tran/tapv.o: tran/tapv.c tran/tapv.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/tapv.c -o tran/tapv.o $(CFLAGS)

tran/biquad.o: tran/biquad.c tran/biquad.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/biquad.c -o tran/biquad.o $(CFLAGS)

tran/pluck.o: tran/pluck.c tran/pluck.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h
	cc -c tran/pluck.c -o tran/pluck.o $(CFLAGS)

nyqsrc/sndfnint.o: nyqsrc/sndfnint.c 
	cc -c nyqsrc/sndfnint.c -o nyqsrc/sndfnint.o $(CFLAGS)

nyqsrc/seqfnint.o: nyqsrc/seqfnint.c 
	cc -c nyqsrc/seqfnint.c -o nyqsrc/seqfnint.o $(CFLAGS)

xlisp/xlftab.o: nyqsrc/sndfnintptrs.h nyqsrc/sndfnintdefs.h nyqsrc/seqfnintptrs.h nyqsrc/seqfnintdefs.h
	cc -c xlisp/xlftab.c -o xlisp/xlftab.o $(CFLAGS)

xlisp/extern.o: xlisp/extern.c
	cc -c xlisp/extern.c -o xlisp/extern.o $(CFLAGS)

xlisp/xldmem.o: xlisp/xldmem.c
	cc -c xlisp/xldmem.c -o xlisp/xldmem.o $(CFLAGS)

xlisp/xlbfun.o: xlisp/xlbfun.c
	cc -c xlisp/xlbfun.c -o xlisp/xlbfun.o $(CFLAGS)

xlisp/xlcont.o: xlisp/xlcont.c
	cc -c xlisp/xlcont.c -o xlisp/xlcont.o $(CFLAGS)

xlisp/xldbug.o: xlisp/xldbug.c
	cc -c xlisp/xldbug.c -o xlisp/xldbug.o $(CFLAGS)

xlisp/xleval.o: xlisp/xleval.c
	cc -c xlisp/xleval.c -o xlisp/xleval.o $(CFLAGS)

xlisp/xlfio.o: xlisp/xlfio.c
	cc -c xlisp/xlfio.c -o xlisp/xlfio.o $(CFLAGS)

xlisp/xlglob.o: xlisp/xlglob.c
	cc -c xlisp/xlglob.c -o xlisp/xlglob.o $(CFLAGS)

xlisp/xlimage.o: xlisp/xlimage.c
	cc -c xlisp/xlimage.c -o xlisp/xlimage.o $(CFLAGS)

xlisp/xlinit.o: xlisp/xlinit.c
	cc -c xlisp/xlinit.c -o xlisp/xlinit.o $(CFLAGS)

xlisp/xlio.o: xlisp/xlio.c
	cc -c xlisp/xlio.c -o xlisp/xlio.o $(CFLAGS)

xlisp/xlisp.o: xlisp/xlisp.c
	cc -c xlisp/xlisp.c -o xlisp/xlisp.o $(CFLAGS)

xlisp/xljump.o: xlisp/xljump.c
	cc -c xlisp/xljump.c -o xlisp/xljump.o $(CFLAGS)

xlisp/xllist.o: xlisp/xllist.c
	cc -c xlisp/xllist.c -o xlisp/xllist.o $(CFLAGS)

xlisp/xlmath.o: xlisp/xlmath.c
	cc -c xlisp/xlmath.c -o xlisp/xlmath.o $(CFLAGS)

xlisp/xlobj.o: xlisp/xlobj.c
	cc -c xlisp/xlobj.c -o xlisp/xlobj.o $(CFLAGS)

xlisp/xlpp.o: xlisp/xlpp.c
	cc -c xlisp/xlpp.c -o xlisp/xlpp.o $(CFLAGS)

xlisp/xlprin.o: xlisp/xlprin.c
	cc -c xlisp/xlprin.c -o xlisp/xlprin.o $(CFLAGS)

xlisp/xlread.o: xlisp/xlread.c
	cc -c xlisp/xlread.c -o xlisp/xlread.o $(CFLAGS)

xlisp/xlstr.o: xlisp/xlstr.c
	cc -c xlisp/xlstr.c -o xlisp/xlstr.o $(CFLAGS)

xlisp/xlsubr.o: xlisp/xlsubr.c
	cc -c xlisp/xlsubr.c -o xlisp/xlsubr.o $(CFLAGS)

xlisp/xlsym.o: xlisp/xlsym.c
	cc -c xlisp/xlsym.c -o xlisp/xlsym.o $(CFLAGS)

xlisp/xlsys.o: xlisp/xlsys.c
	cc -c xlisp/xlsys.c -o xlisp/xlsys.o $(CFLAGS)

cmt/cext.o: cmt/cext.c
	cc -c cmt/cext.c -o cmt/cext.o $(CFLAGS)

cmt/cleanup.o: cmt/cleanup.c
	cc -c cmt/cleanup.c -o cmt/cleanup.o $(CFLAGS)

cmt/cmdline.o: cmt/cmdline.c
	cc -c cmt/cmdline.c -o cmt/cmdline.o $(CFLAGS)

cmt/cmtcmd.o: cmt/cmtcmd.c
	cc -c cmt/cmtcmd.c -o cmt/cmtcmd.o $(CFLAGS)

cmt/moxc.o: cmt/moxc.c
	cc -c cmt/moxc.c -o cmt/moxc.o $(CFLAGS)

cmt/mem.o: cmt/mem.c
	cc -c cmt/mem.c -o cmt/mem.o $(CFLAGS)

cmt/midifile.o: cmt/midifile.c
	cc -c cmt/midifile.c -o cmt/midifile.o $(CFLAGS)

cmt/midifns.o: cmt/midifns.c
	cc -c cmt/midifns.c -o cmt/midifns.o $(CFLAGS)

cmt/record.o: cmt/record.c
	cc -c cmt/record.c -o cmt/record.o $(CFLAGS)

cmt/seq.o: cmt/seq.c
	cc -c cmt/seq.c -o cmt/seq.o $(CFLAGS)

cmt/seqmread.o: cmt/seqmread.c
	cc -c cmt/seqmread.c -o cmt/seqmread.o $(CFLAGS)

cmt/seqmwrite.o: cmt/seqmwrite.c
	cc -c cmt/seqmwrite.c -o cmt/seqmwrite.o $(CFLAGS)

cmt/seqread.o: cmt/seqread.c
	cc -c cmt/seqread.c -o cmt/seqread.o $(CFLAGS)

cmt/seqwrite.o: cmt/seqwrite.c
	cc -c cmt/seqwrite.c -o cmt/seqwrite.o $(CFLAGS)

cmt/tempomap.o: cmt/tempomap.c
	cc -c cmt/tempomap.c -o cmt/tempomap.o $(CFLAGS)

cmt/timebase.o: cmt/timebase.c
	cc -c cmt/timebase.c -o cmt/timebase.o $(CFLAGS)

cmt/userio.o: cmt/userio.c
	cc -c cmt/userio.c -o cmt/userio.o $(CFLAGS)

sys/unix/osstuff.o: sys/unix/osstuff.c
	cc -c sys/unix/osstuff.c -o sys/unix/osstuff.o $(CFLAGS)

misc/intgen: misc/intgen.c
	cd misc; make intgen

misc/unpacker: misc/unpacker.c misc/convert.c
	cd misc; make unpacker

misc/packer: misc/packer.c misc/convert.c
	cd misc; make packer

nyqsrc/sndfnintptrs.h: $(NYQHDRS) snd/snd.h misc/intgen
	$(INTGEN) nyqsrc/sndfnint $(NYQHDRS)

nyqsrc/seqfnintptrs.h: $(CMTHDRS) misc/intgen
	$(INTGEN) nyqsrc/seqfnint $(CMTHDRS)

clean:
	cd misc; make clean
	rm -f $(OBJECTS)
# Note that these files are machine-generated:
	rm -f nyqsrc/sndfnintptrs.h nyqsrc/sndfnint.c nyqsrc/sndfnintdefs.h
	rm -f nyqsrc/seqfnintptrs.h nyqsrc/seqfnint.c nyqsrc/seqfnintdefs.h

cleaner: clean
	cd misc; make cleaner
	rm -f *.backup */*.backup
	rm -f *~ */*.*~
	rm -f *.save */*.save
	rm -f *.CKP */*.CKP
	rm -f *.BAK */*.BAK
	rm -f *.old */*.old
	rm -f *.gold */*.gold
	rm -f playparms
	rm -f points.dat

tar: cleaner
	sh -v sys/unix/cmu/tar.script

backup: cleaner
	sh -v sys/unix/cmu/backup.script
