
add_library( ${TARGET} STATIC )

def_vars()

list( APPEND SOURCES
   PRIVATE
      # libnyquist

      ${TARGET_ROOT}/nyx.c

      # libnyquist/nyquist/cmt

      ${TARGET_ROOT}/nyquist/cmt/cext.c
      ${TARGET_ROOT}/nyquist/cmt/cleanup.c
      ${TARGET_ROOT}/nyquist/cmt/cmdline.c
      ${TARGET_ROOT}/nyquist/cmt/cmtcmd.c
      ${TARGET_ROOT}/nyquist/cmt/mem.c
      ${TARGET_ROOT}/nyquist/cmt/midifile.c
      ${TARGET_ROOT}/nyquist/cmt/midifns.c
      ${TARGET_ROOT}/nyquist/cmt/moxc.c
      ${TARGET_ROOT}/nyquist/cmt/record.c
      ${TARGET_ROOT}/nyquist/cmt/seq.c
      ${TARGET_ROOT}/nyquist/cmt/seqmread.c
      ${TARGET_ROOT}/nyquist/cmt/seqmwrite.c
      ${TARGET_ROOT}/nyquist/cmt/seqread.c
      ${TARGET_ROOT}/nyquist/cmt/seqwrite.c
      ${TARGET_ROOT}/nyquist/cmt/tempomap.c
      ${TARGET_ROOT}/nyquist/cmt/timebase.c
      ${TARGET_ROOT}/nyquist/cmt/userio.c

      # libnyquist/nyquist/cmupv

      ${TARGET_ROOT}/nyquist/cmupv/src/cmupv.c
      ${TARGET_ROOT}/nyquist/cmupv/src/cmupvdbg.c
      ${TARGET_ROOT}/nyquist/cmupv/src/internal.c

      # libnyquist/nyquist/ffts

      ${TARGET_ROOT}/nyquist/ffts/src/fftext.c
      ${TARGET_ROOT}/nyquist/ffts/src/fftlib.c
      ${TARGET_ROOT}/nyquist/ffts/src/matlib.c

      # libnyquist/nyquist/nyqsrc

      ${TARGET_ROOT}/nyquist/nyqsrc/add.c
      ${TARGET_ROOT}/nyquist/nyqsrc/avg.c
      ${TARGET_ROOT}/nyquist/nyqsrc/compose.c
      ${TARGET_ROOT}/nyquist/nyqsrc/convolve.c
      ${TARGET_ROOT}/nyquist/nyqsrc/debug.c
      ${TARGET_ROOT}/nyquist/nyqsrc/downsample.c
      ${TARGET_ROOT}/nyquist/nyqsrc/f0.cpp
      ${TARGET_ROOT}/nyquist/nyqsrc/falloc.c
      ${TARGET_ROOT}/nyquist/nyqsrc/ffilterkit.c
      ${TARGET_ROOT}/nyquist/nyqsrc/fft.c
      ${TARGET_ROOT}/nyquist/nyqsrc/handlers.c
      ${TARGET_ROOT}/nyquist/nyqsrc/inverse.c
      ${TARGET_ROOT}/nyquist/nyqsrc/local.c
      ${TARGET_ROOT}/nyquist/nyqsrc/lpanal.c
      ${TARGET_ROOT}/nyquist/nyqsrc/multiread.c
      ${TARGET_ROOT}/nyquist/nyqsrc/multiseq.c
      ${TARGET_ROOT}/nyquist/nyqsrc/phasevocoder.c
      ${TARGET_ROOT}/nyquist/nyqsrc/probe.c
      ${TARGET_ROOT}/nyquist/nyqsrc/pvshell.c
      ${TARGET_ROOT}/nyquist/nyqsrc/resamp.c
      ${TARGET_ROOT}/nyquist/nyqsrc/resampv.c
      ${TARGET_ROOT}/nyquist/nyqsrc/samples.c
      ${TARGET_ROOT}/nyquist/nyqsrc/seqext.c
      ${TARGET_ROOT}/nyquist/nyqsrc/seqfnint.c
      ${TARGET_ROOT}/nyquist/nyqsrc/seqinterf.c
      ${TARGET_ROOT}/nyquist/nyqsrc/sliderdata.c
      ${TARGET_ROOT}/nyquist/nyqsrc/sndfnint.c
      ${TARGET_ROOT}/nyquist/nyqsrc/sndmax.c
      ${TARGET_ROOT}/nyquist/nyqsrc/sndread.c
      ${TARGET_ROOT}/nyquist/nyqsrc/sndseq.c
      ${TARGET_ROOT}/nyquist/nyqsrc/sndsliders.c
      ${TARGET_ROOT}/nyquist/nyqsrc/sndwritepa.c
      ${TARGET_ROOT}/nyquist/nyqsrc/sound.c
      ${TARGET_ROOT}/nyquist/nyqsrc/stats.c
      ${TARGET_ROOT}/nyquist/nyqsrc/stoponzero.c
      ${TARGET_ROOT}/nyquist/nyqsrc/trigger.c
      ${TARGET_ROOT}/nyquist/nyqsrc/yin.c

      # libnyquist/nyquist/nyqstk

      ${TARGET_ROOT}/nyquist/nyqstk/instr.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/stkinit.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/stkint.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/ADSR.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/BandedWG.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/BiQuad.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/Bowed.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/BowTable.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/Chorus.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/Clarinet.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/Delay.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/DelayA.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/DelayL.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/Effect.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/Envelope.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/FileRead.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/FileWvIn.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/Filter.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/Flute.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/Function.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/Generator.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/Instrmnt.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/JCRev.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/JetTable.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/Mandolin.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/Modal.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/ModalBar.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/Noise.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/NRev.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/OnePole.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/OneZero.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/PitShift.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/PluckTwo.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/PoleZero.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/PRCRev.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/ReedTable.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/Saxofony.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/SineWave.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/Sitar.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/Stk.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/WaveLoop.cpp
      ${TARGET_ROOT}/nyquist/nyqstk/src/WvIn.cpp


      # libnyquist/nyquist/sys

      #${TARGET_ROOT}/nyquist/sys/mac/macaboutbox.c
      #${TARGET_ROOT}/nyquist/sys/mac/MacAE.c
      #${TARGET_ROOT}/nyquist/sys/mac/MacCommandWin.c
      #${TARGET_ROOT}/nyquist/sys/mac/macdrag.c
      #${TARGET_ROOT}/nyquist/sys/mac/MacFileUtils.c
      #${TARGET_ROOT}/nyquist/sys/mac/macfun.c
      #${TARGET_ROOT}/nyquist/sys/mac/MacHandleEv.c
      #${TARGET_ROOT}/nyquist/sys/mac/macint.c
      #${TARGET_ROOT}/nyquist/sys/mac/macstuff.c
      #${TARGET_ROOT}/nyquist/sys/mac/xlextstart.c
      #${TARGET_ROOT}/nyquist/sys/unix/io.c
      #${TARGET_ROOT}/nyquist/sys/unix/osstuff.c
      #${TARGET_ROOT}/nyquist/sys/unix/term.c
      #${TARGET_ROOT}/nyquist/sys/unix/termtest.c
      #${TARGET_ROOT}/nyquist/sys/win/msvc/winfun.c
      #${TARGET_ROOT}/nyquist/sys/win/msvc/winstuff.c
      #${TARGET_ROOT}/nyquist/sys/win/wingui/winguistuff.c
      #${TARGET_ROOT}/nyquist/sys/win/wingui/xlextstart.c
      #${TARGET_ROOT}/nyquist/sys/win/wingui/xlispfns.c

      # libnyquist/nyquist/tran

      ${TARGET_ROOT}/nyquist/tran/abs.c
      ${TARGET_ROOT}/nyquist/tran/allpoles.c
      ${TARGET_ROOT}/nyquist/tran/alpass.c
      ${TARGET_ROOT}/nyquist/tran/alpasscv.c
      ${TARGET_ROOT}/nyquist/tran/alpassvc.c
      ${TARGET_ROOT}/nyquist/tran/alpassvv.c
      ${TARGET_ROOT}/nyquist/tran/amosc.c
      ${TARGET_ROOT}/nyquist/tran/areson.c
      ${TARGET_ROOT}/nyquist/tran/aresoncv.c
      ${TARGET_ROOT}/nyquist/tran/aresonvc.c
      ${TARGET_ROOT}/nyquist/tran/aresonvv.c
      ${TARGET_ROOT}/nyquist/tran/atone.c
      ${TARGET_ROOT}/nyquist/tran/atonev.c
      ${TARGET_ROOT}/nyquist/tran/biquadfilt.c
      ${TARGET_ROOT}/nyquist/tran/buzz.c
      ${TARGET_ROOT}/nyquist/tran/chase.c
      ${TARGET_ROOT}/nyquist/tran/clip.c
      ${TARGET_ROOT}/nyquist/tran/congen.c
      ${TARGET_ROOT}/nyquist/tran/const.c
      ${TARGET_ROOT}/nyquist/tran/coterm.c
      ${TARGET_ROOT}/nyquist/tran/delaycc.c
      ${TARGET_ROOT}/nyquist/tran/delaycv.c
      ${TARGET_ROOT}/nyquist/tran/eqbandvvv.c
      ${TARGET_ROOT}/nyquist/tran/exp.c
      ${TARGET_ROOT}/nyquist/tran/fmfb.c
      ${TARGET_ROOT}/nyquist/tran/fmfbv.c
      ${TARGET_ROOT}/nyquist/tran/fmosc.c
      ${TARGET_ROOT}/nyquist/tran/follow.c
      ${TARGET_ROOT}/nyquist/tran/fromarraystream.c
      ${TARGET_ROOT}/nyquist/tran/fromobject.c
      ${TARGET_ROOT}/nyquist/tran/gate.c
      ${TARGET_ROOT}/nyquist/tran/ifft.c
      ${TARGET_ROOT}/nyquist/tran/instrbanded.c
      ${TARGET_ROOT}/nyquist/tran/instrbow.c
      ${TARGET_ROOT}/nyquist/tran/instrbowedfreq.c
      ${TARGET_ROOT}/nyquist/tran/instrclar.c
      ${TARGET_ROOT}/nyquist/tran/instrclarall.c
      ${TARGET_ROOT}/nyquist/tran/instrclarfreq.c
      ${TARGET_ROOT}/nyquist/tran/instrflute.c
      ${TARGET_ROOT}/nyquist/tran/instrfluteall.c
      ${TARGET_ROOT}/nyquist/tran/instrflutefreq.c
      ${TARGET_ROOT}/nyquist/tran/instrmandolin.c
      ${TARGET_ROOT}/nyquist/tran/instrmodalbar.c
      ${TARGET_ROOT}/nyquist/tran/instrsax.c
      ${TARGET_ROOT}/nyquist/tran/instrsaxall.c
      ${TARGET_ROOT}/nyquist/tran/instrsaxfreq.c
      ${TARGET_ROOT}/nyquist/tran/instrsitar.c
      ${TARGET_ROOT}/nyquist/tran/integrate.c
      ${TARGET_ROOT}/nyquist/tran/log.c
      ${TARGET_ROOT}/nyquist/tran/lpreson.c
      ${TARGET_ROOT}/nyquist/tran/maxv.c
      ${TARGET_ROOT}/nyquist/tran/offset.c
      ${TARGET_ROOT}/nyquist/tran/oneshot.c
      ${TARGET_ROOT}/nyquist/tran/osc.c
      ${TARGET_ROOT}/nyquist/tran/partial.c
      ${TARGET_ROOT}/nyquist/tran/pluck.c
      ${TARGET_ROOT}/nyquist/tran/prod.c
      ${TARGET_ROOT}/nyquist/tran/pwl.c
      ${TARGET_ROOT}/nyquist/tran/quantize.c
      ${TARGET_ROOT}/nyquist/tran/recip.c
      ${TARGET_ROOT}/nyquist/tran/reson.c
      ${TARGET_ROOT}/nyquist/tran/resoncv.c
      ${TARGET_ROOT}/nyquist/tran/resonvc.c
      ${TARGET_ROOT}/nyquist/tran/resonvv.c
      ${TARGET_ROOT}/nyquist/tran/sampler.c
      ${TARGET_ROOT}/nyquist/tran/scale.c
      ${TARGET_ROOT}/nyquist/tran/shape.c
      ${TARGET_ROOT}/nyquist/tran/sine.c
      ${TARGET_ROOT}/nyquist/tran/siosc.c
      ${TARGET_ROOT}/nyquist/tran/slope.c
      ${TARGET_ROOT}/nyquist/tran/sqrt.c
      ${TARGET_ROOT}/nyquist/tran/stkchorus.c
      ${TARGET_ROOT}/nyquist/tran/stkpitshift.c
      ${TARGET_ROOT}/nyquist/tran/stkrev.c
      ${TARGET_ROOT}/nyquist/tran/tapf.c
      ${TARGET_ROOT}/nyquist/tran/tapv.c
      ${TARGET_ROOT}/nyquist/tran/tone.c
      ${TARGET_ROOT}/nyquist/tran/tonev.c
      ${TARGET_ROOT}/nyquist/tran/upsample.c
      ${TARGET_ROOT}/nyquist/tran/white.c

      # libnyquist/nyquist/xlisp

      ${TARGET_ROOT}/nyquist/xlisp/extern.c
      ${TARGET_ROOT}/nyquist/xlisp/path.c
      ${TARGET_ROOT}/nyquist/xlisp/security.c
      ${TARGET_ROOT}/nyquist/xlisp/xlbfun.c
      ${TARGET_ROOT}/nyquist/xlisp/xlcont.c
      ${TARGET_ROOT}/nyquist/xlisp/xldbug.c
      ${TARGET_ROOT}/nyquist/xlisp/xldmem.c
      ${TARGET_ROOT}/nyquist/xlisp/xleval.c
      ${TARGET_ROOT}/nyquist/xlisp/xlfio.c
      ${TARGET_ROOT}/nyquist/xlisp/xlftab.c
      ${TARGET_ROOT}/nyquist/xlisp/xlglob.c
      ${TARGET_ROOT}/nyquist/xlisp/xlimage.c
      ${TARGET_ROOT}/nyquist/xlisp/xlinit.c
      ${TARGET_ROOT}/nyquist/xlisp/xlio.c
      ${TARGET_ROOT}/nyquist/xlisp/xlisp.c
      ${TARGET_ROOT}/nyquist/xlisp/xljump.c
      ${TARGET_ROOT}/nyquist/xlisp/xllist.c
      ${TARGET_ROOT}/nyquist/xlisp/xlmath.c
      ${TARGET_ROOT}/nyquist/xlisp/xlobj.c
      ${TARGET_ROOT}/nyquist/xlisp/xlpp.c
      ${TARGET_ROOT}/nyquist/xlisp/xlprin.c
      ${TARGET_ROOT}/nyquist/xlisp/xlread.c
      ${TARGET_ROOT}/nyquist/xlisp/xlstr.c
      ${TARGET_ROOT}/nyquist/xlisp/xlsubr.c
      ${TARGET_ROOT}/nyquist/xlisp/xlsym.c
      ${TARGET_ROOT}/nyquist/xlisp/xlsys.c
)

list( APPEND INCLUDES
   PRIVATE
      ${TARGET_ROOT}/nyquist/cmt
      ${TARGET_ROOT}/nyquist/cmupv/src
      ${TARGET_ROOT}/nyquist/ffts/src
      ${TARGET_ROOT}/nyquist/nyqsrc
      ${TARGET_ROOT}/nyquist/nyqstk
      ${TARGET_ROOT}/nyquist/nyqstk/include
      ${TARGET_ROOT}/nyquist/tran
      ${TARGET_ROOT}/nyquist/xlisp
      $<$<BOOL:${UNIX}>:${TARGET_ROOT}/nyquist/sys/unix>
      $<$<NOT:$<BOOL:${UNIX}>>:${TARGET_ROOT}/nyquist/sys/win/msvc>
   PUBLIC
      ${TARGET_ROOT}
)

list( APPEND DEFINES
   PRIVATE
      CMTSTUFF
      EXT
      $<$<PLATFORM_ID:Windows>:WIN32>
)

list( APPEND OPTIONS
   PRIVATE
      $<$<PLATFORM_ID:Darwin>:-fno-common>
)

list( APPEND LIBRARIES
   PRIVATE
      portaudio-v19
      libsndfile
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

