set( TARGET libnyquist )

add_library( ${TARGET} STATIC )

def_vars()

set( SOURCES
   PRIVATE
      # libnyquist

      nyx.c

      # libnyquist/nyquist/cmt

      nyquist/cmt/cext.c
      nyquist/cmt/cleanup.c
      nyquist/cmt/cmdline.c
      nyquist/cmt/cmtcmd.c
      nyquist/cmt/mem.c
      nyquist/cmt/midifile.c
      nyquist/cmt/midifns.c
      nyquist/cmt/moxc.c
      nyquist/cmt/record.c
      nyquist/cmt/seq.c
      nyquist/cmt/seqmread.c
      nyquist/cmt/seqmwrite.c
      nyquist/cmt/seqread.c
      nyquist/cmt/seqwrite.c
      nyquist/cmt/tempomap.c
      nyquist/cmt/timebase.c
      nyquist/cmt/userio.c

      # libnyquist/nyquist/cmupv

      nyquist/cmupv/src/cmupv.c
      nyquist/cmupv/src/cmupvdbg.c
      nyquist/cmupv/src/internal.c

      # libnyquist/nyquist/ffts

      nyquist/ffts/src/fftext.c
      nyquist/ffts/src/fftlib.c
      nyquist/ffts/src/matlib.c

      # libnyquist/nyquist/nyqsrc

      nyquist/nyqsrc/add.c
      nyquist/nyqsrc/avg.c
      nyquist/nyqsrc/compose.c
      nyquist/nyqsrc/convolve.c
      nyquist/nyqsrc/debug.c
      nyquist/nyqsrc/downsample.c
      nyquist/nyqsrc/f0.cpp
      nyquist/nyqsrc/falloc.c
      nyquist/nyqsrc/ffilterkit.c
      nyquist/nyqsrc/fft.c
      nyquist/nyqsrc/handlers.c
      nyquist/nyqsrc/inverse.c
      nyquist/nyqsrc/local.c
      nyquist/nyqsrc/lpanal.c
      nyquist/nyqsrc/multiread.c
      nyquist/nyqsrc/multiseq.c
      nyquist/nyqsrc/phasevocoder.c
      nyquist/nyqsrc/probe.c
      nyquist/nyqsrc/pvshell.c
      nyquist/nyqsrc/resamp.c
      nyquist/nyqsrc/resampv.c
      nyquist/nyqsrc/samples.c
      nyquist/nyqsrc/seqext.c
      nyquist/nyqsrc/seqfnint.c
      nyquist/nyqsrc/seqinterf.c
      nyquist/nyqsrc/sliderdata.c
      nyquist/nyqsrc/sndfnint.c
      nyquist/nyqsrc/sndmax.c
      nyquist/nyqsrc/sndread.c
      nyquist/nyqsrc/sndseq.c
      nyquist/nyqsrc/sndsliders.c
      nyquist/nyqsrc/sndwritepa.c
      nyquist/nyqsrc/sound.c
      nyquist/nyqsrc/stats.c
      nyquist/nyqsrc/stoponzero.c
      nyquist/nyqsrc/trigger.c
      nyquist/nyqsrc/yin.c

      # libnyquist/nyquist/nyqstk

      nyquist/nyqstk/instr.cpp
      nyquist/nyqstk/stkinit.cpp
      nyquist/nyqstk/stkint.cpp
      nyquist/nyqstk/src/ADSR.cpp
      nyquist/nyqstk/src/BandedWG.cpp
      nyquist/nyqstk/src/BiQuad.cpp
      nyquist/nyqstk/src/Bowed.cpp
      nyquist/nyqstk/src/BowTable.cpp
      nyquist/nyqstk/src/Chorus.cpp
      nyquist/nyqstk/src/Clarinet.cpp
      nyquist/nyqstk/src/Delay.cpp
      nyquist/nyqstk/src/DelayA.cpp
      nyquist/nyqstk/src/DelayL.cpp
      nyquist/nyqstk/src/Effect.cpp
      nyquist/nyqstk/src/Envelope.cpp
      nyquist/nyqstk/src/FileRead.cpp
      nyquist/nyqstk/src/FileWvIn.cpp
      nyquist/nyqstk/src/Filter.cpp
      nyquist/nyqstk/src/Flute.cpp
      nyquist/nyqstk/src/Function.cpp
      nyquist/nyqstk/src/Generator.cpp
      nyquist/nyqstk/src/Instrmnt.cpp
      nyquist/nyqstk/src/JCRev.cpp
      nyquist/nyqstk/src/JetTable.cpp
      nyquist/nyqstk/src/Mandolin.cpp
      nyquist/nyqstk/src/Modal.cpp
      nyquist/nyqstk/src/ModalBar.cpp
      nyquist/nyqstk/src/Noise.cpp
      nyquist/nyqstk/src/NRev.cpp
      nyquist/nyqstk/src/OnePole.cpp
      nyquist/nyqstk/src/OneZero.cpp
      nyquist/nyqstk/src/PitShift.cpp
      nyquist/nyqstk/src/PluckTwo.cpp
      nyquist/nyqstk/src/PoleZero.cpp
      nyquist/nyqstk/src/PRCRev.cpp
      nyquist/nyqstk/src/ReedTable.cpp
      nyquist/nyqstk/src/Saxofony.cpp
      nyquist/nyqstk/src/SineWave.cpp
      nyquist/nyqstk/src/Sitar.cpp
      nyquist/nyqstk/src/Stk.cpp
      nyquist/nyqstk/src/WaveLoop.cpp
      nyquist/nyqstk/src/WvIn.cpp


      # libnyquist/nyquist/sys

      #nyquist/sys/mac/macaboutbox.c
      #nyquist/sys/mac/MacAE.c
      #nyquist/sys/mac/MacCommandWin.c
      #nyquist/sys/mac/macdrag.c
      #nyquist/sys/mac/MacFileUtils.c
      #nyquist/sys/mac/macfun.c
      #nyquist/sys/mac/MacHandleEv.c
      #nyquist/sys/mac/macint.c
      #nyquist/sys/mac/macstuff.c
      #nyquist/sys/mac/xlextstart.c
      #nyquist/sys/unix/io.c
      #nyquist/sys/unix/osstuff.c
      #nyquist/sys/unix/term.c
      #nyquist/sys/unix/termtest.c
      #nyquist/sys/win/msvc/winfun.c
      #nyquist/sys/win/msvc/winstuff.c
      #nyquist/sys/win/wingui/winguistuff.c
      #nyquist/sys/win/wingui/xlextstart.c
      #nyquist/sys/win/wingui/xlispfns.c

      # libnyquist/nyquist/tran

      nyquist/tran/abs.c
      nyquist/tran/allpoles.c
      nyquist/tran/alpass.c
      nyquist/tran/alpasscv.c
      nyquist/tran/alpassvc.c
      nyquist/tran/alpassvv.c
      nyquist/tran/amosc.c
      nyquist/tran/areson.c
      nyquist/tran/aresoncv.c
      nyquist/tran/aresonvc.c
      nyquist/tran/aresonvv.c
      nyquist/tran/atone.c
      nyquist/tran/atonev.c
      nyquist/tran/biquadfilt.c
      nyquist/tran/buzz.c
      nyquist/tran/chase.c
      nyquist/tran/clip.c
      nyquist/tran/congen.c
      nyquist/tran/const.c
      nyquist/tran/coterm.c
      nyquist/tran/delaycc.c
      nyquist/tran/delaycv.c
      nyquist/tran/eqbandvvv.c
      nyquist/tran/exp.c
      nyquist/tran/fmfb.c
      nyquist/tran/fmfbv.c
      nyquist/tran/fmosc.c
      nyquist/tran/follow.c
      nyquist/tran/fromarraystream.c
      nyquist/tran/fromobject.c
      nyquist/tran/gate.c
      nyquist/tran/ifft.c
      nyquist/tran/instrbanded.c
      nyquist/tran/instrbow.c
      nyquist/tran/instrbowedfreq.c
      nyquist/tran/instrclar.c
      nyquist/tran/instrclarall.c
      nyquist/tran/instrclarfreq.c
      nyquist/tran/instrflute.c
      nyquist/tran/instrfluteall.c
      nyquist/tran/instrflutefreq.c
      nyquist/tran/instrmandolin.c
      nyquist/tran/instrmodalbar.c
      nyquist/tran/instrsax.c
      nyquist/tran/instrsaxall.c
      nyquist/tran/instrsaxfreq.c
      nyquist/tran/instrsitar.c
      nyquist/tran/integrate.c
      nyquist/tran/log.c
      nyquist/tran/lpreson.c
      nyquist/tran/maxv.c
      nyquist/tran/offset.c
      nyquist/tran/oneshot.c
      nyquist/tran/osc.c
      nyquist/tran/partial.c
      nyquist/tran/pluck.c
      nyquist/tran/prod.c
      nyquist/tran/pwl.c
      nyquist/tran/quantize.c
      nyquist/tran/recip.c
      nyquist/tran/reson.c
      nyquist/tran/resoncv.c
      nyquist/tran/resonvc.c
      nyquist/tran/resonvv.c
      nyquist/tran/sampler.c
      nyquist/tran/scale.c
      nyquist/tran/shape.c
      nyquist/tran/sine.c
      nyquist/tran/siosc.c
      nyquist/tran/slope.c
      nyquist/tran/sqrt.c
      nyquist/tran/stkchorus.c
      nyquist/tran/stkpitshift.c
      nyquist/tran/stkrev.c
      nyquist/tran/tapf.c
      nyquist/tran/tapv.c
      nyquist/tran/tone.c
      nyquist/tran/tonev.c
      nyquist/tran/upsample.c
      nyquist/tran/white.c

      # libnyquist/nyquist/xlisp

      nyquist/xlisp/extern.c
      nyquist/xlisp/path.c
      nyquist/xlisp/security.c
      nyquist/xlisp/xlbfun.c
      nyquist/xlisp/xlcont.c
      nyquist/xlisp/xldbug.c
      nyquist/xlisp/xldmem.c
      nyquist/xlisp/xleval.c
      nyquist/xlisp/xlfio.c
      nyquist/xlisp/xlftab.c
      nyquist/xlisp/xlglob.c
      nyquist/xlisp/xlimage.c
      nyquist/xlisp/xlinit.c
      nyquist/xlisp/xlio.c
      nyquist/xlisp/xlisp.c
      nyquist/xlisp/xljump.c
      nyquist/xlisp/xllist.c
      nyquist/xlisp/xlmath.c
      nyquist/xlisp/xlobj.c
      nyquist/xlisp/xlpp.c
      nyquist/xlisp/xlprin.c
      nyquist/xlisp/xlread.c
      nyquist/xlisp/xlstr.c
      nyquist/xlisp/xlsubr.c
      nyquist/xlisp/xlsym.c
      nyquist/xlisp/xlsys.c
)

set( INCLUDES
   PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/nyquist/cmt
      ${CMAKE_CURRENT_SOURCE_DIR}/nyquist/cmupv/src
      ${CMAKE_CURRENT_SOURCE_DIR}/nyquist/ffts/src
      ${CMAKE_CURRENT_SOURCE_DIR}/nyquist/nyqsrc
      ${CMAKE_CURRENT_SOURCE_DIR}/nyquist/nyqstk
      ${CMAKE_CURRENT_SOURCE_DIR}/nyquist/nyqstk/include
      ${CMAKE_CURRENT_SOURCE_DIR}/nyquist/tran
      ${CMAKE_CURRENT_SOURCE_DIR}/nyquist/xlisp
      $<$<BOOL:${UNIX}>:${CMAKE_CURRENT_SOURCE_DIR}/nyquist/sys/unix>
      $<$<NOT:$<BOOL:${UNIX}>>:${CMAKE_CURRENT_SOURCE_DIR}/nyquist/sys/win/msvc>
   PUBLIC
      ${CMAKE_CURRENT_SOURCE_DIR}
)

set( DEFINES
   PUBLIC
      USE_NYQUIST=1
   PRIVATE
      CMTSTUFF
      EXT
      $<$<PLATFORM_ID:Windows>:WIN32>
)

set( OPTIONS
   PRIVATE
      $<$<PLATFORM_ID:Darwin>:-fno-common>
)

set( LIBRARIES
   PRIVATE
      portaudio::portaudio
      SndFile::sndfile
)

set_target_properties( ${TARGET} PROPERTIES FOLDER "lib-src" )
set_target_properties( ${TARGET} PROPERTIES POSITION_INDEPENDENT_CODE On)

organize_source( "${CMAKE_CURRENT_SOURCE_DIR}" "" "${SOURCES}" )

target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )
