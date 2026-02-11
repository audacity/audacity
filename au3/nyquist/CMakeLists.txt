#[[
Nyquist runtime files (.lsp and .raw files) for Audacity.

This CMakeLists.txt handles:
1. Installing runtime files for release builds
2. Copying runtime files to the build directory for development builds
]]


set( TARGET nyquist )

message( STATUS "========== Configuring ${TARGET} ==========" )

# Determine install and build destinations based on platform
if(OS_IS_MAC)
   # On macOS, Nyquist runtime files go in Contents/Resources/nyquist
   set(NYQUIST_INSTALL_DEST "${AU4_SHARE_NAME}${AU4_INSTALL_NAME}")
   set(NYQUIST_BUILD_DEST "${CMAKE_BINARY_DIR}/src/app/${AU4_SHARE_NAME}${AU4_INSTALL_NAME}")
elseif(OS_IS_LIN)
   # On Linux, files go in share/audacity/ which PathList.cpp expects
   set(NYQUIST_INSTALL_DEST "${CMAKE_INSTALL_PREFIX}/share/audacity")
   set(NYQUIST_BUILD_DEST "${CMAKE_BINARY_DIR}/share/audacity")
else()
   set(NYQUIST_INSTALL_DEST "${CMAKE_INSTALL_PREFIX}")
   set(NYQUIST_BUILD_DEST "${CMAKE_BINARY_DIR}/")
endif()

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
   set( src "${CMAKE_CURRENT_SOURCE_DIR}/${source}" )
   set( dst "${NYQUIST_BUILD_DEST}${TARGET}/${source}" )

   # Fix this when reorganizing the Nyquist sources
   if( source STREQUAL "system.lsp" )
      if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
         set( src "${CMAKE_SOURCE_DIR}/au3/lib-src/libnyquist/nyquist/sys/win/msvc/system.lsp" )
      endif()
   endif()

   add_custom_command(
      DEPENDS
         "${src}"
      COMMAND
         ${CMAKE_COMMAND} -E make_directory "${NYQUIST_BUILD_DEST}${TARGET}"
      COMMAND
         ${CMAKE_COMMAND} -E copy "${src}" "${dst}"
      OUTPUT
         "${dst}"
   )

   list( APPEND SOURCES "${src}" )
   list( APPEND OUTPUTS "${dst}" )
endforeach()

add_custom_target( ${TARGET} ALL DEPENDS ${OUTPUTS} SOURCES ${SOURCES} )

# Install Nyquist runtime files for release builds
if( NOT CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   if( NOT "${CMAKE_GENERATOR}" MATCHES "Visual Studio*")
      install( DIRECTORY "${NYQUIST_BUILD_DEST}${TARGET}"
               DESTINATION "${NYQUIST_INSTALL_DEST}" )
   endif()
endif()

