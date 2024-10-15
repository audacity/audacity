
set( TARGET nyquist )
set( TARGET_ROOT ${topdir}/nyquist )

message( STATUS "========== Configuring ${TARGET} ==========" )

def_vars()

set( RUNTIME
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
   nyquist-plot.txt
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

foreach( source ${RUNTIME} )
   set( src "${_SRCDIR}/${source}" )
   set( dst "${_DEST}/${TARGET}/${source}" )

   # Fix this when reorganizing the Nyquist sources
   if( source STREQUAL "system.lsp" )
      if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
         set( src "${libsrc}/libnyquist/nyquist/sys/win/msvc/system.lsp" )
      endif()
   endif()

   add_custom_command(
      DEPENDS
         "${src}"
      COMMAND
         ${CMAKE_COMMAND} -E make_directory "${_DEST}/${TARGET}"
      COMMAND
         ${CMAKE_COMMAND} -E copy "${src}" "${dst}"
      OUTPUT
         "${dst}"
   )

   list( APPEND SOURCES "${src}" )
   list( APPEND OUTPUTS "${dst}" )
endforeach()

add_custom_target( ${TARGET} ALL DEPENDS ${OUTPUTS} SOURCES ${SOURCES} )

if( NOT CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   if( NOT "${CMAKE_GENERATOR}" MATCHES "Visual Studio*")
      install( DIRECTORY "${_DEST}/${TARGET}"
               DESTINATION "${_PKGDATA}" )
   endif()
endif()

