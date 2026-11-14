#[[
Nyquist plug-ins (.ny files) for Audacity.

This CMakeLists.txt handles:
1. Installing nyquist-plug-ins for release builds
2. Copying nyquist-plug-ins to the build directory for development builds
]]

set(TARGET nyquist-plug-ins)

# List of Nyquist plug-in files
set(PLUGIN_FILES
   ShelfFilter.ny
   SpectralEditMulti.ny
   SpectralEditParametricEQ.ny
   SpectralEditShelves.ny
   StudioFadeOut.ny
   adjustable-fade.ny
   beat.ny
   clipfix.ny
   crossfadeclips.ny
   crossfadetracks.ny
   delay.ny
   equalabel.ny
   highpass.ny
   label-sounds.ny
   legacy-limiter.ny
   lowpass.ny
   noisegate.ny
   notch.ny
   nyquist-plug-in-installer.ny
   pluck.ny
   rhythmtrack.ny
   rissetdrum.ny
   rms.ny
   sample-data-export.ny
   sample-data-import.ny
   spectral-delete.ny
   tremolo.ny
   vocoder.ny
)

# Determine install and build destinations based on platform
if(OS_IS_MAC)
   # On macOS, nyquist-plug-ins go in Contents/nyquist-plug-ins (not Contents/Resources/nyquist-plug-ins)
   # This matches what PluginManager.cpp expects (see lines 229-238)
   set(PLUGINS_INSTALL_DEST "${AU4_SHARE_NAME}Contents/nyquist-plug-ins")
   set(PLUGINS_BUILD_DEST "${CMAKE_BINARY_DIR}/src/app/${AU4_SHARE_NAME}Contents/nyquist-plug-ins")
elseif(OS_IS_LIN)
   # On Linux, files go in share/audacity/nyquist-plug-ins which PathList.cpp expects
   set(PLUGINS_INSTALL_DEST "${CMAKE_INSTALL_PREFIX}/share/audacity/nyquist-plug-ins")
   set(PLUGINS_BUILD_DEST "${CMAKE_BINARY_DIR}/share/audacity/nyquist-plug-ins")
else()
   set(PLUGINS_INSTALL_DEST "${AU4_SHARE_NAME}${AU4_INSTALL_NAME}bin/nyquist-plug-ins")
   set(PLUGINS_BUILD_DEST "${CMAKE_BINARY_DIR}/${AU4_SHARE_NAME}${AU4_INSTALL_NAME}nyquist-plug-ins")
endif()

# Install nyquist-plug-ins for release builds
install(FILES
   ${PLUGIN_FILES}
   DESTINATION ${PLUGINS_INSTALL_DEST}
)

# Copy nyquist-plug-ins to build directory for development builds
# Build list of source and output files
foreach( plugin ${PLUGIN_FILES} )
   set( src "${CMAKE_CURRENT_SOURCE_DIR}/${plugin}" )
   set( dst "${PLUGINS_BUILD_DEST}/${plugin}" )

   add_custom_command(
      DEPENDS
         "${src}"
      COMMAND
         ${CMAKE_COMMAND} -E make_directory "${PLUGINS_BUILD_DEST}"
      COMMAND
         ${CMAKE_COMMAND} -E copy "${src}" "${dst}"
      OUTPUT
         "${dst}"
   )

   list( APPEND SOURCES "${src}" )
   list( APPEND OUTPUTS "${dst}" )
endforeach()

add_custom_target( ${TARGET} ALL DEPENDS ${OUTPUTS} SOURCES ${SOURCES} )
