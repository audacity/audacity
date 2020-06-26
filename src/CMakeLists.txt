
set( TARGET Audacity )
set( TARGET_ROOT ${topdir}/src )

message( STATUS "========== Configuring ${TARGET} ==========" )

# Allow modules to link against the Audacity executable
set( CMAKE_ENABLE_EXPORTS ON )

# add sub-directory libraries to the build
foreach( SUBDIRECTORY
   effect-classes
   effects
   etc
   export
   import
   labeltrack
   menus
   notetrack
   ondemand
   playabletrack
   prefs
   timetrack
   toolbars
   track
   wavetrack
   widgets
   xml
)
   add_subdirectory( "${SUBDIRECTORY}" )
endforeach()

# add the executable, including contents of object libraries in subdirectories
add_executable( ${TARGET} ${COLLECTED_OBJECTS} )

if( COLLECTED_MODULES )
   add_dependencies( ${TARGET} ${COLLECTED_MODULES} )
endif()
add_dependencies( ${TARGET} locale )
add_dependencies( ${TARGET} nyquist )
add_dependencies( ${TARGET} plug-ins )

def_vars()

# Handle Audio Units option
if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   cmd_option(
      ${_OPT}use_audio_units
      "Use Audio Units plug-in support [on, off]"
      ON
   )
   set( USE_AUDIO_UNITS ${${_OPT}use_audio_units} CACHE INTERNAL "" )
endif()

# Handle Ladspa option
cmd_option(
   ${_OPT}use_ladspa
   "Use LADSPA plug-in support [on, off]"
   ON
)
set( USE_LADSPA ${${_OPT}use_ladspa} CACHE INTERNAL "" )

# Handle VST option
cmd_option(
   ${_OPT}use_vst
   "Use VST2 plug-in support [on, off]"
   ON
)
set( USE_VST ${${_OPT}use_vst} CACHE INTERNAL "" )

#
#
#
list( APPEND SOURCES
   PRIVATE
      Audacity.h
      AudacityHeaders.h.in
      Experimental.h

      # Blockfile

      blockfile/LegacyAliasBlockFile.cpp
      blockfile/LegacyAliasBlockFile.h
      blockfile/LegacyBlockFile.cpp
      blockfile/LegacyBlockFile.h
      blockfile/NotYetAvailableException.cpp
      blockfile/NotYetAvailableException.h
      blockfile/ODDecodeBlockFile.cpp
      blockfile/ODDecodeBlockFile.h
      blockfile/ODPCMAliasBlockFile.cpp
      blockfile/ODPCMAliasBlockFile.h
      blockfile/PCMAliasBlockFile.cpp
      blockfile/PCMAliasBlockFile.h
      blockfile/SilentBlockFile.cpp
      blockfile/SilentBlockFile.h
      blockfile/SimpleBlockFile.cpp
      blockfile/SimpleBlockFile.h

      # Commands

      commands/AppCommandEvent.cpp
      commands/AppCommandEvent.h
      commands/AudacityCommand.cpp
      commands/AudacityCommand.h
      commands/BatchEvalCommand.cpp
      commands/BatchEvalCommand.h
      commands/Command.cpp
      commands/Command.h
      commands/CommandBuilder.cpp
      commands/CommandBuilder.h
      commands/CommandContext.cpp
      commands/CommandContext.h
      commands/CommandDirectory.cpp
      commands/CommandDirectory.h
      commands/CommandFlag.h
      commands/CommandFunctors.h
      commands/CommandHandler.cpp
      commands/CommandHandler.h
      commands/CommandManager.cpp
      commands/CommandManager.h
      commands/CommandManagerWindowClasses.h
      commands/CommandMisc.h
      commands/CommandSignature.cpp
      commands/CommandSignature.h
      commands/CommandTargets.cpp
      commands/CommandTargets.h
      commands/CommandType.cpp
      commands/CommandType.h
      commands/CompareAudioCommand.cpp
      commands/CompareAudioCommand.h
      commands/Demo.cpp
      commands/Demo.h
      commands/DragCommand.cpp
      commands/DragCommand.h
      commands/GetInfoCommand.cpp
      commands/GetInfoCommand.h
      commands/GetTrackInfoCommand.cpp
      commands/GetTrackInfoCommand.h
      commands/HelpCommand.cpp
      commands/HelpCommand.h
      commands/ImportExportCommands.cpp
      commands/ImportExportCommands.h
      commands/Keyboard.cpp
      commands/Keyboard.h
      commands/LoadCommands.cpp
      commands/LoadCommands.h
      commands/MessageCommand.cpp
      commands/MessageCommand.h
      commands/OpenSaveCommands.cpp
      commands/OpenSaveCommands.h
      commands/PreferenceCommands.cpp
      commands/PreferenceCommands.h
      commands/ResponseQueue.cpp
      commands/ResponseQueue.h
      commands/ScreenshotCommand.cpp
      commands/ScreenshotCommand.h
      commands/ScriptCommandRelay.cpp
      commands/ScriptCommandRelay.h
      commands/SelectCommand.cpp
      commands/SelectCommand.h
      commands/SetClipCommand.cpp
      commands/SetClipCommand.h
      commands/SetEnvelopeCommand.cpp
      commands/SetEnvelopeCommand.h
      commands/SetLabelCommand.cpp
      commands/SetLabelCommand.h
      commands/SetProjectCommand.cpp
      commands/SetProjectCommand.h
      commands/SetTrackInfoCommand.cpp
      commands/SetTrackInfoCommand.h
      commands/Validators.h

)

#
#
#
list( APPEND HEADERS
   ../include/audacity/EffectInterface.h
   ../include/audacity/Types.h
   ../include/audacity/ConfigInterface.h
   ../include/audacity/ModuleInterface.h
   ../include/audacity/PluginInterface.h
   ../include/audacity/ComponentInterface.h
   ../include/audacity/EffectAutomationParameters.h
   ../include/audacity/ImporterInterface.h
)

#
# Define resources
#
list( APPEND RESOURCES
   ../presets/EQDefaultCurves.xml
)

#
#
#
list( APPEND LIBRARIES
   PRIVATE
      ${CMAKE_REQUIRED_LIBRARIES}
      wxWidgets
      libsndfile
)

set( BUILDING_AUDACITY YES )
set( INSTALL_PREFIX "${_PREFIX}" )
set( PKGLIBDIR "${_LIBDIR}" )
set( LIBDIR "${_LIBDIR}" )
set( HAVE_GTK ${GTK_FOUND} )

# Do not define these for Windows or Mac until further testing
# can be done against ExportMP3.cpp.  If either are defined, the
# build will fail.
if( NOT CMAKE_SYSTEM_NAME MATCHES "Windows|Darwin" )
   if( "${${_OPT}use_lame}" STREQUAL "local" )
      set( DISABLE_DYNAMIC_LOADING_LAME YES )
   endif()

   if( "${${_OPT}use_ffmpeg}" STREQUAL "linked" )
      set( DISABLE_DYNAMIC_LOADING_FFMPEG YES )
   endif()
endif()

if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   set_target_properties(
      ${TARGET}
      PROPERTIES
         # Make sure we're a GUI application and not a console tool
         WIN32_EXECUTABLE ON
   )

   # Define the Windows specific resources
   list( APPEND WIN_RESOURCES
      ../win/audacity.rc
   )

   # Create the config file
   configure_file( audacity_config.h.in private/configwin.h )

   # Copy over the wxWidgets DLLs
   if( ${_OPT}use_wxwidgets STREQUAL "system" )
      set( wxlibs "$ENV{WXWIN}" )
   else()
      set( wxlibs "${CMAKE_BINARY_DIR}" )
   endif()

   # Convert the paths to native
   file( TO_NATIVE_PATH "${_INTDIR}/dlls" dlls )
   file( TO_NATIVE_PATH "${wxWidgets_LIB_DIR}" libdir )
   file( TO_NATIVE_PATH "${_EXEDIR}" exedir )

   # And create the script to copy the WX libs to the exeutable directory
   file( WRITE "${_INTDIR}/copy_libs.bat"
      "@ECHO OFF
      IF NOT %1 EQU xyzzy (
         IF EXIST \"${dlls}\" DEL \"${dlls}\"
         CALL %0 xyzzy \"%1\" %2
         FOR /F \"delims=\" %%c IN ('SORT \"${dlls}\"') DO (
            IF NOT EXIST \"%1\"\\%%c (
               xcopy \"${libdir}\"\\%%c \"%1\"
            )
         )
         DEL ${dlls}
      )
      IF %1 EQU xyzzy (
         FOR /F %%i IN ('DUMPBIN /DEPENDENTS \"%2\"\\%3 ^| findstr /B \"/C:    wx\"') DO (
            ECHO %%i >>\"${dlls}\"
            CALL %0 xyzzy \"${libdir}\" %%i
         )
      )"
   )

   # Add it to the build
   add_custom_command(
      TARGET
         ${TARGET}
      COMMAND
         ${_INTDIR}/copy_libs.bat ${exedir} ${AUDACITY_NAME}.exe
      POST_BUILD
   )

   # Copy the VC runtime libraries as well
   add_custom_command(
      TARGET
         ${TARGET}
      COMMAND
         ${CMAKE_COMMAND} -E copy ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS} ${_DEST}
      POST_BUILD
   )
elseif( CMAKE_SYSTEM_NAME MATCHES "Darwin" )

   # Define Mac specific resources
   list( APPEND MAC_RESOURCES
      ../mac/Resources/Audacity.icns
      ../mac/Resources/AudacityAIFF.icns
      ../mac/Resources/AudacityAU.icns
      ../mac/Resources/AudacityMP3.icns
      ../mac/Resources/AudacityOGG.icns
      ../mac/Resources/AudacityProject.icns
      ../mac/Resources/AudacityWAV.icns
   )

   set_target_properties(
      ${TARGET}
      PROPERTIES
         # Build a bundle
         MACOSX_BUNDLE ON
         # Add the resources
         RESOURCE "${MAC_RESOURCES};${RESOURCES}"
   )
   set_target_property_all( ${TARGET} RUNTIME_OUTPUT_DIRECTORY "${_DESTDIR}" )

   # Add our required frameworks
   list( APPEND LIBRARIES
      PRIVATE
         "-framework AudioUnit"
         "-framework CoreAudio"
         "-framework CoreAudioKit"
   )

   # Use the Aqua theme
   set( USE_AQUA_THEME 1 )

   # Create the config file
   set( HAVE_VISIBILITY 1 )
   configure_file( audacity_config.h.in private/configmac.h )

   # Copy the wxWidgets libraries into the bundle
   if( "${CMAKE_GENERATOR}" MATCHES "Xcode" )
      add_custom_command(
         TARGET
            ${TARGET}
         COMMAND
            sh -c "TARGET_BUILD_DIR=${_DEST} EXECUTABLE_PATH=MacOS/${AUDACITY_NAME} FRAMEWORKS_FOLDER_PATH=Frameworks ${topdir}/mac/scripts/install_wxlibs.sh"
         POST_BUILD
      )
   endif()

   # Define the Wrapper target
   set( WRAPPER_ROOT "${TARGET_ROOT}/../mac" )
   set( WRAPPER_SOURCES "${WRAPPER_ROOT}/Wrapper.c" )

   add_executable( Wrapper "${WRAPPER_SOURCES}" )
   add_dependencies( "${TARGET}" Wrapper )

   set_target_property_all( "Wrapper" RUNTIME_OUTPUT_DIRECTORY "${_EXEDIR}" )
   organize_source( "${WRAPPER_ROOT}" "mac" "${WRAPPER_SOURCES}" )

else()
   set_target_property_all( ${TARGET} RUNTIME_OUTPUT_DIRECTORY "${_DESTDIR}" )

   # Create the config file
   set( HAVE_VISIBILITY 1 )
   configure_file( audacity_config.h.in private/configunix.h )

   # Build the list of mimetypes
   #
   # (Don't use generator expressions since it will leave null/empty
   # entries in the list.)
   if( USE_FFMPEG )
      list( APPEND MIMETYPES
         audio/aac
         audio/ac3
         audio/mp4
         audio/x-ms-wma
         video/mpeg
      )
   endif()
   if( USE_LIBFLAC )
      list( APPEND MIMETYPES
         audio/flac
         audio/x-flac
      )
   endif()
   if( USE_LIBMAD )
      list( APPEND MIMETYPES
         audio/mpeg
      )
   endif()
   if( USE_SNDFILE )
      list( APPEND MIMETYPES
         audio/basic
         audio/x-aiff
         audio/x-wav
      )
   endif()
   if( USE_LIBOGG AND USE_LIBVORBIS )
      list( APPEND MIMETYPES
         application/ogg
         audio/x-vorbis+ogg
      )
   endif()

   # Create the desktop file
   configure_file( audacity.desktop.in ${_INTDIR}/audacity.desktop )

   # Create the script to copy required wxWidgets libraries
   if( ${_OPT}use_wxwidgets STREQUAL "local" )
      file( WRITE "${_INTDIR}/copy_libs.sh"
         "for lib in \$(ldd ${_EXEDIR}/${AUDACITY_NAME} | awk '/libwx/{print \$1}')
          do
             echo \${lib}
             ldd ${WXWIN}/lib/\${lib} | awk '/libwx/{print \$1}'
          done | sort -u | xargs cp -n -H -t ${_LIBDIR}
          rm \${0}"
      )

      # And run it after the build
      add_custom_command(
         TARGET
            ${TARGET}
         COMMAND
            sh "${_INTDIR}/copy_libs.sh"
         WORKING_DIRECTORY
            ${WXWIN}/lib
         POST_BUILD
      )
   endif()
endif()

set_target_property_all( ${TARGET} RUNTIME_OUTPUT_NAME ${AUDACITY_NAME} )

organize_source( "${TARGET_ROOT}/.." "include" "${HEADERS}" )
organize_source( "${TARGET_ROOT}/../presets" "presets" "${RESOURCES}" )
organize_source( "${TARGET_ROOT}" "src" "${SOURCES}" )
organize_source( "${TARGET_ROOT}/../mac/Resources" "mac" "${MAC_RESOURCES}" )
organize_source( "${TARGET_ROOT}/../win" "win" "${WIN_RESOURCES}" )

#
# Get those pesky CMake files out of the way (on Windows)
#
source_group(
    TREE
        "${_INTDIR}"
    PREFIX
        "CMake"
    FILES
        ${_INTDIR}/CMakeFiles/Audacity.dir/cmake_pch.cxx
        ${_INTDIR}/CMakeFiles/Audacity.dir/cmake_pch.hxx
)

target_sources( ${TARGET} PRIVATE ${HEADERS} ${SOURCES} ${RESOURCES} ${MAC_RESOURCES} ${WIN_RESOURCES} )

target_include_directories( ${TARGET} PRIVATE
   etc
   export
   effects
   labeltrack
   ondemand
   prefs
   toolbars
   track
   wavetrack
   widgets
   widgets/FileDialog
)

# If was have cmake 3.16 or higher, we can use precompiled headers, but
# only use them if ccache is not available and the user hasn't disabled
# it.
if( CMAKE_VERSION VERSION_GREATER_EQUAL "3.16" AND NOT CCACHE_PROGRAM )
   cmd_option(
      ${_OPT}use_pch
      "Use precompiled headers [yes, no]"
      YES
   )

   if( ${_OPT}use_pch )
      message( STATUS "Using precompiled headers" )
   else()
      message( STATUS "Not using precompiled headers" )
   endif()
endif()

# link to static libraries in subfolders
target_link_libraries( ${TARGET} PRIVATE ${COLLECTED_LIBRARIES} )

# link other libraries
audacity_target_options( ${TARGET} "${LIBRARIES}" "" )

if( NOT "${CMAKE_GENERATOR}" MATCHES "Xcode|Visual Studio*" )
   if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
      install( FILES "${_DEST}/Info.plist"
               DESTINATION "${CMAKE_INSTALL_BINDIR}/Audacity.app/Contents" )
      install( PROGRAMS "${_EXEDIR}/${AUDACITY_NAME}" "${_EXEDIR}/Wrapper"
               DESTINATION "${CMAKE_INSTALL_BINDIR}/Audacity.app/Contents/MacOS" )
   else()
      install( PROGRAMS "${_EXEDIR}/${AUDACITY_NAME}"
               TYPE BIN )
      install( DIRECTORY "${_DESTDIR}/${_LIBDIR}/"
               DESTINATION "${_LIBDIR}"
               USE_SOURCE_PERMISSIONS
               FILES_MATCHING PATTERN "*.so" )
      install( FILES "${_INTDIR}/audacity.desktop"
               DESTINATION "${_DATADIR}/applications" )
      install( FILES "${topdir}/LICENSE.txt" "${topdir}/README.txt"
               DESTINATION "${_DATADIR}/doc/${AUDACITY_NAME}" )
      install( FILES "${_SRCDIR}/audacity.xml"
               DESTINATION "${_DATADIR}/mime/packages" )
      install( FILES "${topdir}/presets/EQDefaultCurves.xml"
               DESTINATION "${_PKGDATA}" )
   endif()
endif()

