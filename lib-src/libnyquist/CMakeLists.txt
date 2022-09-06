set( TARGET libnyquist )

add_library( ${TARGET} STATIC )

def_vars()

list( APPEND SOURCES
   PRIVATE
      # libnyquist

      nyx.c
      nyx.h

      # libnyquist/nyquist/cmt

      nyquist/cmt/cext.c
      nyquist/cmt/cext.h
      nyquist/cmt/cleanup.c
      nyquist/cmt/cleanup.h
      nyquist/cmt/cmdline.c
      nyquist/cmt/cmdline.h
      nyquist/cmt/cmtcmd.c
      nyquist/cmt/cmtcmd.h
      nyquist/cmt/hashrout.h
      nyquist/cmt/mem.c
      nyquist/cmt/mem.h
      nyquist/cmt/midifile.c
      nyquist/cmt/midifile.h
      nyquist/cmt/midifns.c
      nyquist/cmt/midifns.h
      nyquist/cmt/moxc.c
      nyquist/cmt/moxc.h
      nyquist/cmt/record.c
      nyquist/cmt/record.h
      nyquist/cmt/seq.c
      nyquist/cmt/seq.h
      nyquist/cmt/seqmread.c
      nyquist/cmt/seqmread.h
      nyquist/cmt/seqmwrite.c
      nyquist/cmt/seqmwrite.h
      nyquist/cmt/seqread.c
      nyquist/cmt/seqread.h
      nyquist/cmt/seqwrite.c
      nyquist/cmt/seqwrite.h
      nyquist/cmt/tempomap.c
      nyquist/cmt/tempomap.h
      nyquist/cmt/timebase.c
      nyquist/cmt/timebase.h
      nyquist/cmt/userio.c
      nyquist/cmt/userio.h

      # libnyquist/nyquist/cmupv

      nyquist/cmupv/src/cmupv.c
      nyquist/cmupv/src/cmupv.h
      nyquist/cmupv/src/cmupvdbg.c
      nyquist/cmupv/src/cmupvdbg.h
      nyquist/cmupv/src/internal.c
      nyquist/cmupv/src/internal.h

      # libnyquist/nyquist/ffts

      nyquist/ffts/src/fftext.c
      nyquist/ffts/src/fftext.h
      nyquist/ffts/src/fftlib.c
      nyquist/ffts/src/fftlib.h
      nyquist/ffts/src/matlib.c
      nyquist/ffts/src/matlib.h

      # libnyquist/nyquist/nyqsrc

      nyquist/nyqsrc/add.c
      nyquist/nyqsrc/add.h
      nyquist/nyqsrc/avg.c
      nyquist/nyqsrc/avg.h
      nyquist/nyqsrc/compose.c
      nyquist/nyqsrc/compose.h
      nyquist/nyqsrc/convolve.c
      nyquist/nyqsrc/convolve.h
      nyquist/nyqsrc/debug.c
      nyquist/nyqsrc/debug.h
      nyquist/nyqsrc/downsample.c
      nyquist/nyqsrc/downsample.h
      nyquist/nyqsrc/f0.cpp
      nyquist/nyqsrc/f0.h
      nyquist/nyqsrc/falloc.c
      nyquist/nyqsrc/falloc.h
      nyquist/nyqsrc/ffilterkit.c
      nyquist/nyqsrc/ffilterkit.h
      nyquist/nyqsrc/fft.c
      nyquist/nyqsrc/fft.h
      nyquist/nyqsrc/handlers.c
      nyquist/nyqsrc/inverse.c
      nyquist/nyqsrc/inverse.h
      nyquist/nyqsrc/local.c
      nyquist/nyqsrc/localdefs.h
      nyquist/nyqsrc/lpanal.c
      nyquist/nyqsrc/lpanal.h
      nyquist/nyqsrc/multiread.c
      nyquist/nyqsrc/multiread.h
      nyquist/nyqsrc/multiseq.c
      nyquist/nyqsrc/multiseq.h
      nyquist/nyqsrc/phasevocoder.c
      nyquist/nyqsrc/phasevocoder.h
      nyquist/nyqsrc/probe.c
      nyquist/nyqsrc/probe.h
      nyquist/nyqsrc/pvshell.c
      nyquist/nyqsrc/pvshell.h
      nyquist/nyqsrc/resamp.c
      nyquist/nyqsrc/resamp.h
      nyquist/nyqsrc/resampv.c
      nyquist/nyqsrc/resampv.h
      nyquist/nyqsrc/samples.c
      nyquist/nyqsrc/samples.h
      nyquist/nyqsrc/seqext.c
      nyquist/nyqsrc/seqext.h
      nyquist/nyqsrc/seqfnint.c
      nyquist/nyqsrc/seqfnintdefs.h
      nyquist/nyqsrc/seqfnintptrs.h
      nyquist/nyqsrc/seqinterf.c
      nyquist/nyqsrc/seqinterf.h
      nyquist/nyqsrc/sliderdata.c
      nyquist/nyqsrc/sliderdata.h
      nyquist/nyqsrc/sndfnint.c
      nyquist/nyqsrc/sndfnintdefs.h
      nyquist/nyqsrc/sndfnintptrs.h
      nyquist/nyqsrc/sndmax.c
      nyquist/nyqsrc/sndmax.h
      nyquist/nyqsrc/sndread.c
      nyquist/nyqsrc/sndread.h
      nyquist/nyqsrc/sndseq.c
      nyquist/nyqsrc/sndseq.h
      nyquist/nyqsrc/sndsliders.c
      nyquist/nyqsrc/sndsliders.h
      nyquist/nyqsrc/sndwritepa.c
      nyquist/nyqsrc/sndwritepa.h
      nyquist/nyqsrc/sound.c
      nyquist/nyqsrc/sound.h
      nyquist/nyqsrc/stats.c
      nyquist/nyqsrc/stoponzero.c
      nyquist/nyqsrc/stoponzero.h
      nyquist/nyqsrc/trigger.c
      nyquist/nyqsrc/trigger.h
      nyquist/nyqsrc/yin.c
      nyquist/nyqsrc/yin.h

      # libnyquist/nyquist/nyqstk

      nyquist/nyqstk/globals.h
      nyquist/nyqstk/instr.cpp
      nyquist/nyqstk/instr.h
      nyquist/nyqstk/stkinit.cpp
      nyquist/nyqstk/stkinit.h
      nyquist/nyqstk/stkint.cpp
      nyquist/nyqstk/stkint.h
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
      nyquist/tran/abs.h
      nyquist/tran/allpoles.c
      nyquist/tran/allpoles.h
      nyquist/tran/alpass.c
      nyquist/tran/alpass.h
      nyquist/tran/alpasscv.c
      nyquist/tran/alpasscv.h
      nyquist/tran/alpassvc.c
      nyquist/tran/alpassvc.h
      nyquist/tran/alpassvv.c
      nyquist/tran/alpassvv.h
      nyquist/tran/amosc.c
      nyquist/tran/amosc.h
      nyquist/tran/areson.c
      nyquist/tran/areson.h
      nyquist/tran/aresoncv.c
      nyquist/tran/aresoncv.h
      nyquist/tran/aresonvc.c
      nyquist/tran/aresonvc.h
      nyquist/tran/aresonvv.c
      nyquist/tran/aresonvv.h
      nyquist/tran/atone.c
      nyquist/tran/atone.h
      nyquist/tran/atonev.c
      nyquist/tran/atonev.h
      nyquist/tran/biquadfilt.c
      nyquist/tran/biquadfilt.h
      nyquist/tran/buzz.c
      nyquist/tran/buzz.h
      nyquist/tran/chase.c
      nyquist/tran/chase.h
      nyquist/tran/clip.c
      nyquist/tran/clip.h
      nyquist/tran/congen.c
      nyquist/tran/congen.h
      nyquist/tran/const.c
      nyquist/tran/const.h
      nyquist/tran/coterm.c
      nyquist/tran/coterm.h
      nyquist/tran/delaycc.c
      nyquist/tran/delaycc.h
      nyquist/tran/delaycv.c
      nyquist/tran/delaycv.h
      nyquist/tran/eqbandvvv.c
      nyquist/tran/eqbandvvv.h
      nyquist/tran/exp.c
      nyquist/tran/exp.h
      nyquist/tran/fmfb.c
      nyquist/tran/fmfb.h
      nyquist/tran/fmfbv.c
      nyquist/tran/fmfbv.h
      nyquist/tran/fmosc.c
      nyquist/tran/fmosc.h
      nyquist/tran/follow.c
      nyquist/tran/follow.h
      nyquist/tran/fromarraystream.c
      nyquist/tran/fromarraystream.h
      nyquist/tran/fromobject.c
      nyquist/tran/fromobject.h
      nyquist/tran/gate.c
      nyquist/tran/gate.h
      nyquist/tran/ifft.c
      nyquist/tran/ifft.h
      nyquist/tran/instrbanded.c
      nyquist/tran/instrbanded.h
      nyquist/tran/instrbow.c
      nyquist/tran/instrbow.h
      nyquist/tran/instrbowedfreq.c
      nyquist/tran/instrbowedfreq.h
      nyquist/tran/instrclar.c
      nyquist/tran/instrclar.h
      nyquist/tran/instrclarall.c
      nyquist/tran/instrclarall.h
      nyquist/tran/instrclarfreq.c
      nyquist/tran/instrclarfreq.h
      nyquist/tran/instrflute.c
      nyquist/tran/instrflute.h
      nyquist/tran/instrfluteall.c
      nyquist/tran/instrfluteall.h
      nyquist/tran/instrflutefreq.c
      nyquist/tran/instrflutefreq.h
      nyquist/tran/instrmandolin.c
      nyquist/tran/instrmandolin.h
      nyquist/tran/instrmodalbar.c
      nyquist/tran/instrmodalbar.h
      nyquist/tran/instrsax.c
      nyquist/tran/instrsax.h
      nyquist/tran/instrsaxall.c
      nyquist/tran/instrsaxall.h
      nyquist/tran/instrsaxfreq.c
      nyquist/tran/instrsaxfreq.h
      nyquist/tran/instrsitar.c
      nyquist/tran/instrsitar.h
      nyquist/tran/integrate.c
      nyquist/tran/integrate.h
      nyquist/tran/log.c
      nyquist/tran/log.h
      nyquist/tran/lpreson.c
      nyquist/tran/lpreson.h
      nyquist/tran/maxv.c
      nyquist/tran/maxv.h
      nyquist/tran/offset.c
      nyquist/tran/offset.h
      nyquist/tran/oneshot.c
      nyquist/tran/oneshot.h
      nyquist/tran/osc.c
      nyquist/tran/osc.h
      nyquist/tran/partial.c
      nyquist/tran/partial.h
      nyquist/tran/pluck.c
      nyquist/tran/pluck.h
      nyquist/tran/prod.c
      nyquist/tran/prod.h
      nyquist/tran/pwl.c
      nyquist/tran/pwl.h
      nyquist/tran/quantize.c
      nyquist/tran/quantize.h
      nyquist/tran/recip.c
      nyquist/tran/recip.h
      nyquist/tran/reson.c
      nyquist/tran/reson.h
      nyquist/tran/resoncv.c
      nyquist/tran/resoncv.h
      nyquist/tran/resonvc.c
      nyquist/tran/resonvc.h
      nyquist/tran/resonvv.c
      nyquist/tran/resonvv.h
      nyquist/tran/sampler.c
      nyquist/tran/sampler.h
      nyquist/tran/scale.c
      nyquist/tran/scale.h
      nyquist/tran/shape.c
      nyquist/tran/shape.h
      nyquist/tran/sine.c
      nyquist/tran/sine.h
      nyquist/tran/siosc.c
      nyquist/tran/siosc.h
      nyquist/tran/slope.c
      nyquist/tran/slope.h
      nyquist/tran/sqrt.c
      nyquist/tran/sqrt.h
      nyquist/tran/stkchorus.c
      nyquist/tran/stkchorus.h
      nyquist/tran/stkpitshift.c
      nyquist/tran/stkpitshift.h
      nyquist/tran/stkrev.c
      nyquist/tran/stkrev.h
      nyquist/tran/tapf.c
      nyquist/tran/tapf.h
      nyquist/tran/tapv.c
      nyquist/tran/tapv.h
      nyquist/tran/tone.c
      nyquist/tran/tone.h
      nyquist/tran/tonev.c
      nyquist/tran/tonev.h
      nyquist/tran/upsample.c
      nyquist/tran/upsample.h
      nyquist/tran/white.c
      nyquist/tran/white.h

      # libnyquist/nyquist/xlisp

      nyquist/xlisp/extern.c
      nyquist/xlisp/extern.h
      nyquist/xlisp/osdefs.h
      nyquist/xlisp/osptrs.h
      nyquist/xlisp/path.c
      nyquist/xlisp/security.c
      nyquist/xlisp/xlbfun.c
      nyquist/xlisp/xlcont.c
      nyquist/xlisp/xldbug.c
      nyquist/xlisp/xldmem.c
      nyquist/xlisp/xldmem.h
      nyquist/xlisp/xleval.c
      nyquist/xlisp/xlfio.c
      nyquist/xlisp/xlftab.c
      nyquist/xlisp/xlglob.c
      nyquist/xlisp/xlimage.c
      nyquist/xlisp/xlinit.c
      nyquist/xlisp/xlio.c
      nyquist/xlisp/xlisp.c
      nyquist/xlisp/xlisp.h
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

list( APPEND INCLUDES
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

list( APPEND DEFINES
   PUBLIC
      USE_NYQUIST=1
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
      PortAudio::PortAudio
      SndFile::sndfile
)

set_target_properties( ${TARGET} PROPERTIES FOLDER "lib-src" )

organize_source( "${CMAKE_CURRENT_SOURCE_DIR}" "" "${SOURCES}" )

target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )
