
set( TARGET nyquist )
set( TARGET_ROOT ${topdir}/nyquist )

message( STATUS "========== Configuring ${TARGET} ==========" )

def_vars()

list( APPEND SOURCES
   aud-do-support.lsp
   dspprims.lsp
   envelopes.lsp
   equalizer.lsp
   evalenv.lsp
   fileio.lsp
   init.lsp
   misc.lsp
   nyinit-dbg.lsp
   nyinit.lsp
   nyqmisc.lsp
   nyquist.lsp
   printrec.lsp
   profile.lsp
   sal-parse.lsp
   sal.lsp
   seq.lsp
   seqfnint.lsp
   seqmidi.lsp
   sliders.lsp
   sndfnint.lsp
   spec-plot.lsp
   spectral-analysis.lsp
   stk.lsp
   system.lsp
   test.lsp
   velocity.lsp
   xlinit.lsp
   xm.lsp
   rawwaves/mand1.raw
   rawwaves/mand10.raw
   rawwaves/mand11.raw
   rawwaves/mand12.raw
   rawwaves/mand2.raw
   rawwaves/mand3.raw
   rawwaves/mand4.raw
   rawwaves/mand5.raw
   rawwaves/mand6.raw
   rawwaves/mand7.raw
   rawwaves/mand8.raw
   rawwaves/mand9.raw
   rawwaves/mandpluk.raw
   rawwaves/marmstk1.raw
   rawwaves/sinewave.raw
)

foreach( source ${SOURCES} )
   set( src "${_SRCDIR}/${source}" )
   set( dst "${_EXEDIR}/${TARGET}/${source}" )

   add_custom_command(
      DEPENDS
         "${src}"
      COMMAND
         ${CMAKE_COMMAND} -E make_directory "${_EXEDIR}/${TARGET}"
      COMMAND
         ${CMAKE_COMMAND} -E copy "${src}" "${dst}"
      OUTPUT
         "${dst}"
   )

   list( APPEND OUTPUTS "${dst}" )
endforeach()

add_custom_target( ${TARGET} ALL DEPENDS ${OUTPUTS} SOURCES ${SOURCES} )

