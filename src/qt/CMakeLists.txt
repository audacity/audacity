include(AudacityQt5)

set(QAUDACITY QAudacity)

set(QAUDACITY_BINDIR "${CMAKE_BINARY_DIR}/bin/qt")

set(SOURCES
   main.cpp
)

set(RESOURCES

)

set(QTRESOURCES audacity.qrc)

add_executable(${QAUDACITY})

if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   set_target_properties(
      ${QAUDACITY}
      PROPERTIES
         WIN32_EXECUTABLE ON
   )

   list( APPEND WIN_RESOURCES
      #${CMAKE_SOURCE_DIR}/win/audacity.rc
   )

   add_custom_command(
      TARGET
         ${QAUDACITY}
      COMMAND
         ${CMAKE_COMMAND} -D SRC="${QAUDACITY_BINDIR}/$<CONFIG>/QAudacity.exe"
                          -D DST="${QAUDACITY_BINDIR}/$<CONFIG>"
                          -D WXWIN="${_SHARED_PROXY_BASE_PATH}/$<CONFIG>/"
                          -D PLUGINS="${QAUDACITY_BINDIR}/$<CONFIG>/qml/**/*.dll"
                          -P ${AUDACITY_MODULE_PATH}/CopyLibs.cmake
      POST_BUILD
   )

   # Copy the VC runtime libraries as well
   add_custom_command(
      TARGET
         ${QAUDACITY}
      COMMAND
         ${CMAKE_COMMAND} -E copy ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS} ${QAUDACITY_BINDIR}/$<CONFIG>
      POST_BUILD
   )

   set_target_property_all( ${QAUDACITY} RUNTIME_OUTPUT_DIRECTORY "${QAUDACITY_BINDIR}/$<CONFIG>" )
elseif( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   configure_file("macos/Info.plist.in" "${CMAKE_BINARY_DIR}/QAudacityInfo.plist")
   # Define Mac specific resources
   list( APPEND MAC_RESOURCES
      ${CMAKE_SOURCE_DIR}/mac/Resources/Audacity.icns
      ${CMAKE_SOURCE_DIR}/mac/Resources/AudacityAIFF.icns
      ${CMAKE_SOURCE_DIR}/mac/Resources/AudacityAU.icns
      ${CMAKE_SOURCE_DIR}/mac/Resources/AudacityMP3.icns
      ${CMAKE_SOURCE_DIR}/mac/Resources/AudacityOGG.icns
      ${CMAKE_SOURCE_DIR}/mac/Resources/AudacityProject.icns
      ${CMAKE_SOURCE_DIR}/mac/Resources/AudacityWAV.icns
   )

   set_target_properties(
      ${QAUDACITY}
      PROPERTIES
         # Build a bundle
         MACOSX_BUNDLE ON
         MACOSX_BUNDLE_INFO_PLIST "${CMAKE_BINARY_DIR}/QAudacityInfo.plist"
         # Add the resources
         RESOURCE "${MAC_RESOURCES};${RESOURCES}"
   )

   # Add our required frameworks
   list( APPEND LIBRARIES
      PRIVATE
         "-framework AudioUnit"
         "-framework CoreAudio"
         "-framework CoreAudioKit"
   )

   # Copy the required wxWidgets libs into the bundle
   add_custom_command(
      TARGET
         ${QAUDACITY}
      COMMAND
         ${CMAKE_COMMAND} -D SRC="${QAUDACITY_BINDIR}/$<CONFIG>/QAudacity.app/Contents/MacOS/QAudacity"
                          -D DST="${QAUDACITY_BINDIR}/$<CONFIG>/QAudacity.app/Contents/Frameworks"
                          -D WXWIN="${_SHARED_PROXY_BASE_PATH}/$<CONFIG>"
                          -D PLUGINS="${QAUDACITY_BINDIR}/$<CONFIG>/QAudacity.app/Contents/PlugIns/**/*.dylib"
                          -P ${AUDACITY_MODULE_PATH}/CopyLibs.cmake
      POST_BUILD
   )

   set_target_property_all( ${QAUDACITY} RUNTIME_OUTPUT_DIRECTORY "${QAUDACITY_BINDIR}/$<CONFIG>" )
else()

   # Build the list of mimetypes
   #
   # (Don't use generator expressions since it will leave null/empty
   # entries in the list.)
   set( MIMETYPES
      application/x-audacity-project
   )
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
   set( AUDCITY_NAME QAudacity )
   configure_file( ../audacity.desktop.in ${CMAKE_CURRENT_BINARY_DIR}/qaudacity.desktop )

   # Copy the required wxWidgets libs into the bundle
   add_custom_command(
      TARGET
         ${QAUDACITY}
      COMMAND
         ${CMAKE_COMMAND} -D SRC="${QAUDACITY_BINDIR}/$<CONFIG>/bin/QAudacity"
                          -D DST="${QAUDACITY_BINDIR}/$<CONFIG>/lib/qaudacity"
                          -D WXWIN="${_SHARED_PROXY_BASE_PATH}/$<CONFIG>"
                          -D PLUGINS="${QAUDACITY_BINDIR}/$<CONFIG>/lib/qaudacity/qt/**/*.so"
                          -P ${AUDACITY_MODULE_PATH}/CopyLibs.cmake
      POST_BUILD
   )

   file(MAKE_DIRECTORY "${QAUDACITY_BINDIR}/$<CONFIG>/bin")
   set_target_property_all( ${QAUDACITY} RUNTIME_OUTPUT_DIRECTORY "${QAUDACITY_BINDIR}/$<CONFIG>/bin" )
endif()

list( APPEND LIBRARIES Qt5::Core Qt5::Gui Qt5::Widgets Qt5::Quick )

set( QTSOURCES )

foreach( resource ${QTRESOURCES} )
   audacity_qt_add_resource( QTSOURCES ${resource} )
endforeach()

target_sources( ${QAUDACITY} PRIVATE ${SOURCES} ${RESOURCES} ${MAC_RESOURCES} ${WIN_RESOURCES} ${QTRESOURCES} ${QTSOURCES} )
target_compile_definitions( ${QAUDACITY} PRIVATE ${DEFINES} )
target_compile_options( ${QAUDACITY} PRIVATE ${OPTIONS} )
target_include_directories( ${QAUDACITY} PRIVATE ${INCLUDES} )
target_link_options( ${QAUDACITY} PRIVATE ${LDFLAGS} )
target_link_libraries( ${QAUDACITY} PUBLIC ${AUDACITY_LIBRARIES} )
target_link_libraries( ${QAUDACITY} ${LIBRARIES} )
