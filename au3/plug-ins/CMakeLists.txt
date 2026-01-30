#[[
Nyquist plug-ins (.ny files) for Audacity.

This CMakeLists.txt handles:
1. Installing plug-ins for release builds
2. Copying plug-ins to the build directory for development builds
]]

set(TARGET plug-ins)

message(STATUS "========== Configuring ${TARGET} ==========")

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
   # On macOS, plug-ins go in Contents/plug-ins (not Contents/Resources/plug-ins)
   # This matches what PluginManager.cpp expects (see lines 229-238)
   set(PLUGINS_INSTALL_DEST "${AU4_SHARE_NAME}Contents/plug-ins")
   set(PLUGINS_BUILD_DEST "${CMAKE_BINARY_DIR}/src/app/${AU4_SHARE_NAME}Contents/plug-ins")
else()
   set(PLUGINS_INSTALL_DEST "${AU4_SHARE_NAME}${AU4_INSTALL_NAME}plug-ins")
   set(PLUGINS_BUILD_DEST "${CMAKE_BINARY_DIR}/bin/${AU4_SHARE_NAME}${AU4_INSTALL_NAME}plug-ins")
endif()

# Install plug-ins for release builds
install(DIRECTORY
   ${CMAKE_CURRENT_SOURCE_DIR}/
   DESTINATION ${PLUGINS_INSTALL_DEST}
   FILES_MATCHING
   PATTERN "*.ny"
   PATTERN "CMakeLists.txt" EXCLUDE
   PATTERN ".*" EXCLUDE
)

# Copy plug-ins to build directory for development builds
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
