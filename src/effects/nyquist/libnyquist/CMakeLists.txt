set( TARGET libnyquist )

add_library( ${TARGET} STATIC )

get_property(_nyq GLOBAL PROPERTY nyquist_SOURCE_DIR)
set(NYQUIST_ROOT ${_nyq}/nyquist)

set( SOURCES
   PRIVATE
      # libnyquist
      nyx.c

      # libnyquist/nyquist/cmt
      ${NYQUIST_ROOT}/nyquist/cmt/cext.c
      ${NYQUIST_ROOT}/nyquist/cmt/cleanup.c
      ${NYQUIST_ROOT}/nyquist/cmt/cmdline.c
      ${NYQUIST_ROOT}/nyquist/cmt/cmtcmd.c
      ${NYQUIST_ROOT}/nyquist/cmt/mem.c
      ${NYQUIST_ROOT}/nyquist/cmt/midifile.c
      ${NYQUIST_ROOT}/nyquist/cmt/midifns.c
      ${NYQUIST_ROOT}/nyquist/cmt/moxc.c
      ${NYQUIST_ROOT}/nyquist/cmt/record.c
      ${NYQUIST_ROOT}/nyquist/cmt/seq.c
      ${NYQUIST_ROOT}/nyquist/cmt/seqmread.c
      ${NYQUIST_ROOT}/nyquist/cmt/seqmwrite.c
      ${NYQUIST_ROOT}/nyquist/cmt/seqread.c
      ${NYQUIST_ROOT}/nyquist/cmt/seqwrite.c
      ${NYQUIST_ROOT}/nyquist/cmt/tempomap.c
      ${NYQUIST_ROOT}/nyquist/cmt/timebase.c
      ${NYQUIST_ROOT}/nyquist/cmt/userio.c

      # libnyquist/nyquist/cmupv
      ${NYQUIST_ROOT}/nyquist/cmupv/src/cmupv.c
      ${NYQUIST_ROOT}/nyquist/cmupv/src/cmupvdbg.c
      ${NYQUIST_ROOT}/nyquist/cmupv/src/internal.c

      # libnyquist/nyquist/ffts
      ${NYQUIST_ROOT}/nyquist/ffts/src/fftext.c
      ${NYQUIST_ROOT}/nyquist/ffts/src/fftlib.c
      ${NYQUIST_ROOT}/nyquist/ffts/src/matlib.c

      # libnyquist/nyquist/nyqsrc
      ${NYQUIST_ROOT}/nyquist/nyqsrc/add.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/avg.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/compose.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/convolve.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/debug.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/downsample.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/f0.cpp
      ${NYQUIST_ROOT}/nyquist/nyqsrc/falloc.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/ffilterkit.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/fft.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/handlers.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/inverse.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/local.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/lpanal.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/multiread.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/multiseq.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/phasevocoder.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/probe.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/pvshell.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/resamp.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/resampv.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/samples.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/seqext.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/seqfnint.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/seqinterf.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/sliderdata.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/sndfnint.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/sndmax.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/sndread.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/sndseq.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/sndsliders.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/sndwritepa.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/sound.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/stats.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/stoponzero.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/trigger.c
      ${NYQUIST_ROOT}/nyquist/nyqsrc/yin.c

      # libnyquist/nyquist/nyqstk
      ${NYQUIST_ROOT}/nyquist/nyqstk/instr.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/stkinit.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/stkint.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/ADSR.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/BandedWG.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/BiQuad.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/Bowed.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/BowTable.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/Chorus.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/Clarinet.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/Delay.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/DelayA.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/DelayL.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/Effect.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/Envelope.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/FileRead.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/FileWvIn.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/Filter.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/Flute.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/Function.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/Generator.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/Instrmnt.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/JCRev.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/JetTable.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/Mandolin.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/Modal.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/ModalBar.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/Noise.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/NRev.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/OnePole.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/OneZero.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/PitShift.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/PluckTwo.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/PoleZero.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/PRCRev.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/ReedTable.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/Saxofony.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/SineWave.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/Sitar.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/Stk.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/WaveLoop.cpp
      ${NYQUIST_ROOT}/nyquist/nyqstk/src/WvIn.cpp


      # libnyquist/nyquist/sys
      # those files have been removed from SOURCES in 2021
      # but apparently there are still some INCLUDES dir setup below with some of them
      # also strangely nyquist-runtime needs nyquist/sys/win/msvc/system.lsp
      # this would need more investigation before removing them completely
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
      ${NYQUIST_ROOT}/nyquist/tran/abs.c
      ${NYQUIST_ROOT}/nyquist/tran/allpoles.c
      ${NYQUIST_ROOT}/nyquist/tran/alpass.c
      ${NYQUIST_ROOT}/nyquist/tran/alpasscv.c
      ${NYQUIST_ROOT}/nyquist/tran/alpassvc.c
      ${NYQUIST_ROOT}/nyquist/tran/alpassvv.c
      ${NYQUIST_ROOT}/nyquist/tran/amosc.c
      ${NYQUIST_ROOT}/nyquist/tran/areson.c
      ${NYQUIST_ROOT}/nyquist/tran/aresoncv.c
      ${NYQUIST_ROOT}/nyquist/tran/aresonvc.c
      ${NYQUIST_ROOT}/nyquist/tran/aresonvv.c
      ${NYQUIST_ROOT}/nyquist/tran/atone.c
      ${NYQUIST_ROOT}/nyquist/tran/atonev.c
      ${NYQUIST_ROOT}/nyquist/tran/biquadfilt.c
      ${NYQUIST_ROOT}/nyquist/tran/buzz.c
      ${NYQUIST_ROOT}/nyquist/tran/chase.c
      ${NYQUIST_ROOT}/nyquist/tran/clip.c
      ${NYQUIST_ROOT}/nyquist/tran/congen.c
      ${NYQUIST_ROOT}/nyquist/tran/const.c
      ${NYQUIST_ROOT}/nyquist/tran/coterm.c
      ${NYQUIST_ROOT}/nyquist/tran/delaycc.c
      ${NYQUIST_ROOT}/nyquist/tran/delaycv.c
      ${NYQUIST_ROOT}/nyquist/tran/eqbandvvv.c
      ${NYQUIST_ROOT}/nyquist/tran/exp.c
      ${NYQUIST_ROOT}/nyquist/tran/fmfb.c
      ${NYQUIST_ROOT}/nyquist/tran/fmfbv.c
      ${NYQUIST_ROOT}/nyquist/tran/fmosc.c
      ${NYQUIST_ROOT}/nyquist/tran/follow.c
      ${NYQUIST_ROOT}/nyquist/tran/fromarraystream.c
      ${NYQUIST_ROOT}/nyquist/tran/fromobject.c
      ${NYQUIST_ROOT}/nyquist/tran/gate.c
      ${NYQUIST_ROOT}/nyquist/tran/ifft.c
      ${NYQUIST_ROOT}/nyquist/tran/instrbanded.c
      ${NYQUIST_ROOT}/nyquist/tran/instrbow.c
      ${NYQUIST_ROOT}/nyquist/tran/instrbowedfreq.c
      ${NYQUIST_ROOT}/nyquist/tran/instrclar.c
      ${NYQUIST_ROOT}/nyquist/tran/instrclarall.c
      ${NYQUIST_ROOT}/nyquist/tran/instrclarfreq.c
      ${NYQUIST_ROOT}/nyquist/tran/instrflute.c
      ${NYQUIST_ROOT}/nyquist/tran/instrfluteall.c
      ${NYQUIST_ROOT}/nyquist/tran/instrflutefreq.c
      ${NYQUIST_ROOT}/nyquist/tran/instrmandolin.c
      ${NYQUIST_ROOT}/nyquist/tran/instrmodalbar.c
      ${NYQUIST_ROOT}/nyquist/tran/instrsax.c
      ${NYQUIST_ROOT}/nyquist/tran/instrsaxall.c
      ${NYQUIST_ROOT}/nyquist/tran/instrsaxfreq.c
      ${NYQUIST_ROOT}/nyquist/tran/instrsitar.c
      ${NYQUIST_ROOT}/nyquist/tran/integrate.c
      ${NYQUIST_ROOT}/nyquist/tran/log.c
      ${NYQUIST_ROOT}/nyquist/tran/lpreson.c
      ${NYQUIST_ROOT}/nyquist/tran/maxv.c
      ${NYQUIST_ROOT}/nyquist/tran/offset.c
      ${NYQUIST_ROOT}/nyquist/tran/oneshot.c
      ${NYQUIST_ROOT}/nyquist/tran/osc.c
      ${NYQUIST_ROOT}/nyquist/tran/partial.c
      ${NYQUIST_ROOT}/nyquist/tran/pluck.c
      ${NYQUIST_ROOT}/nyquist/tran/prod.c
      ${NYQUIST_ROOT}/nyquist/tran/pwl.c
      ${NYQUIST_ROOT}/nyquist/tran/quantize.c
      ${NYQUIST_ROOT}/nyquist/tran/recip.c
      ${NYQUIST_ROOT}/nyquist/tran/reson.c
      ${NYQUIST_ROOT}/nyquist/tran/resoncv.c
      ${NYQUIST_ROOT}/nyquist/tran/resonvc.c
      ${NYQUIST_ROOT}/nyquist/tran/resonvv.c
      ${NYQUIST_ROOT}/nyquist/tran/sampler.c
      ${NYQUIST_ROOT}/nyquist/tran/scale.c
      ${NYQUIST_ROOT}/nyquist/tran/shape.c
      ${NYQUIST_ROOT}/nyquist/tran/sine.c
      ${NYQUIST_ROOT}/nyquist/tran/siosc.c
      ${NYQUIST_ROOT}/nyquist/tran/slope.c
      ${NYQUIST_ROOT}/nyquist/tran/sqrt.c
      ${NYQUIST_ROOT}/nyquist/tran/stkchorus.c
      ${NYQUIST_ROOT}/nyquist/tran/stkpitshift.c
      ${NYQUIST_ROOT}/nyquist/tran/stkrev.c
      ${NYQUIST_ROOT}/nyquist/tran/tapf.c
      ${NYQUIST_ROOT}/nyquist/tran/tapv.c
      ${NYQUIST_ROOT}/nyquist/tran/tone.c
      ${NYQUIST_ROOT}/nyquist/tran/tonev.c
      ${NYQUIST_ROOT}/nyquist/tran/upsample.c
      ${NYQUIST_ROOT}/nyquist/tran/white.c

      # libnyquist/nyquist/xlisp
      ${NYQUIST_ROOT}/nyquist/xlisp/extern.c
      ${NYQUIST_ROOT}/nyquist/xlisp/path.c
      ${NYQUIST_ROOT}/nyquist/xlisp/security.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlbfun.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlcont.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xldbug.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xldmem.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xleval.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlfio.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlftab.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlglob.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlimage.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlinit.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlio.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlisp.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xljump.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xllist.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlmath.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlobj.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlpp.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlprin.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlread.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlstr.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlsubr.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlsym.c
      ${NYQUIST_ROOT}/nyquist/xlisp/xlsys.c
)

set( INCLUDES
   PRIVATE
      ${NYQUIST_ROOT}/nyquist/cmt
      ${NYQUIST_ROOT}/nyquist/cmupv/src
      ${NYQUIST_ROOT}/nyquist/ffts/src
      ${NYQUIST_ROOT}/nyquist/nyqsrc
      ${NYQUIST_ROOT}/nyquist/nyqstk
      ${NYQUIST_ROOT}/nyquist/nyqstk/include
      ${NYQUIST_ROOT}/nyquist/tran
      ${NYQUIST_ROOT}/nyquist/xlisp
      $<$<BOOL:${UNIX}>:${NYQUIST_ROOT}/nyquist/sys/unix>
      $<$<NOT:$<BOOL:${UNIX}>>:${NYQUIST_ROOT}/nyquist/sys/win/msvc>
   PUBLIC
      ${CMAKE_CURRENT_SOURCE_DIR}
      # consumers include "nyquist/..." paths from the populated tree
      ${NYQUIST_ROOT}
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
      $<$<CXX_COMPILER_ID:AppleClang,Clang,GNU>:-w>
      $<$<CXX_COMPILER_ID:MSVC>:/w>
)

set( LIBRARIES
   PRIVATE
      portaudio::portaudio
      SndFile::sndfile
)

list( APPEND PROPERTIES
    FOLDER "thirdparty"
    POSITION_INDEPENDENT_CODE On
)

set_target_properties( ${TARGET} PROPERTIES ${PROPERTIES} )

target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )

# current nyquist version does not compile on modern compilers that defautls to c23
# forcing gnu17
target_compile_options( ${TARGET} PRIVATE $<$<COMPILE_LANG_AND_ID:C,GNU,Clang,AppleClang>:-std=gnu17> )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )
