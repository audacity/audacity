set(QAUDACITY ${CMAKE_PROJECT_NAME})

set(APP_SOURCES
   AdornedQtRulerPanel.cpp
   AdornedQtRulerPanel.h
   EditToolbarHandler.cpp
   EditToolbarHandler.h
   ExtraMenu.cpp
   ExtraMenu.h
   MasterVolumeToolbarHandler.cpp
   MasterVolumeToolbarHandler.h
   TimeToolbarHandler.cpp
   TimeToolbarHandler.h
   TranslationManager.cpp
   TranslationManager.h
   TransportToolbarHandler.cpp
   TransportToolbarHandler.h
)

set(RESOURCES
   fonts/fonts.qrc
)

qt_add_resources(QT_RESOURCES
   ${RESOURCES}
)

add_subdirectory(uicomponents)
add_subdirectory(uithemes)
add_subdirectory(numeric-formats)
add_subdirectory(trackpanel)

set(QT_TRANSLATION_SOURCES
   translations/de.ts
)

qt_add_executable(${QAUDACITY} MANUAL_FINALIZATION)
target_sources(${QAUDACITY} PRIVATE main.cpp ${QT_RESOURCES})

qt_add_translations(
   ${QAUDACITY}
   RESOURCE_PREFIX "/translations"
   TS_FILES ${QT_TRANSLATION_SOURCES}
)

set(QML_SOURCES
   qml/main.qml
   qml/AddNewTrackPanel.qml
   qml/CustomiseToolbar.qml
   qml/EditToolbar.qml
   qml/MasterVolumeToolbar.qml
   qml/ScrollableRuler.qml
   qml/Sidebar.qml
   qml/SnappingButton.qml
   qml/TimelineRuler.qml
   qml/TimeToolbar.qml
   qml/ToolsToolbar.qml
   qml/TransportToolbar.qml
   qml/Workspace.qml
)

audacity_qml_module(
	${QAUDACITY}
	URI Audacity
	VERSION 1.0
   DEPENDENCIES
      QtQuick
      Audacity.UiComponents
      Audacity.UiThemes
      Audacity.NumericFormats
      Audacity.TrackPanel
   QML_FILES
      ${QML_SOURCES}
   SOURCES
      ${APP_SOURCES}
   NO_RESOURCE_TARGET_PATH
   LIBRARIES
      Qt6::Core
      Qt6::Gui
      Qt6::Quick
      KDAB::kddockwidgets
      uicomponentsplugin
      uithemesplugin
      numeric-formatsplugin
      trackpanelplugin
      lib-qt-init
      lib-project
      lib-audio-io
      lib-numeric-formats
      lib-time-frequency-selection
      lib-track
      lib-wave-track
      lib-project-file-io
)

set_target_properties(
      ${QAUDACITY}
      PROPERTIES
         WIN32_EXECUTABLE ON
         MACOSX_BUNDLE ON
)

if(CMAKE_SYSTEM_NAME MATCHES "Darwin")
   set(AUDACITY_MACOS_EXECUTABLE_NAME "Audacity")

   if(NOT XCODE)
      set_target_properties(
         ${QAUDACITY}
         PROPERTIES
            RUNTIME_OUTPUT_DIRECTORY "${_DESTDIR}"
      )
   endif()

   #fix_bundle(${QAUDACITY})
elseif(NOT CMAKE_SYSTEM_NAME MATCHES "Darwin|Windows")
   set_target_properties(
      ${QAUDACITY}
      PROPERTIES
         RUNTIME_OUTPUT_DIRECTORY "${_DESTDIR}/bin"
         RUNTIME_OUTPUT_NAME "audacity"
   )
endif()

target_compile_definitions( ${QAUDACITY}
   PRIVATE
      $<$<CONFIG:Debug>:QT_QML_DEBUG>
      -DAUDACITY_QML_RESOURCE_PREFIX="${AUDACITY_QML_RESOURCE_PREFIX}")

qt_finalize_target(${QAUDACITY})

organize_source( "${CMAKE_CURRENT_SOURCE_DIR}" "" "${SOURCES}" )
organize_source( "${CMAKE_CURRENT_SOURCE_DIR}" "res" "${RESOURCES}" )
organize_source( "${CMAKE_CURRENT_SOURCE_DIR}" "" "${QML_SOURCES}" )

if(CMAKE_SYSTEM_NAME MATCHES "Windows")
   find_file(D3D_COMPILER
      NAMES
         D3DCompiler_47.dll
         D3DCompiler_43.dll
         D3DCompiler_42.dll
      PATHS $<IF:$<BOOL:${IS_64BIT}>,"C:\\Windows\\System32","C:\\Windows\\SysWOW64">
   )

   install(CODE
      "execute_process(\
         COMMAND \"${PYTHON}\" \"${CMAKE_SOURCE_DIR}/scripts/build/deploy_windows.py\"\
         --executable \"$<TARGET_FILE:${QAUDACITY}>\"\
         --source \"$<TARGET_FILE_DIR:${QAUDACITY}>\"\
         --dest \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}\"\
         --qrc-files \"${CMAKE_CURRENT_SOURCE_DIR}/audacity.qrc\"\
      )"
   )

   if( D3D_COMPILER )
      install(FILES ${D3D_COMPILER} DESTINATION .)
   endif()

   install(DIRECTORY "$<TARGET_FILE_DIR:${QAUDACITY}>/plug-ins" DESTINATION .)
   install(DIRECTORY "$<TARGET_FILE_DIR:${QAUDACITY}>/nyquist" DESTINATION .)
elseif(CMAKE_SYSTEM_NAME MATCHES "Darwin")
   install(CODE
      "execute_process(\
         COMMAND \"${PYTHON}\" \"${CMAKE_SOURCE_DIR}/scripts/build/deploy_macos.py\"\
         --source \"${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/Audacity.app\"\
         --dest \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}\"\
         --qrc-files \"${CMAKE_CURRENT_SOURCE_DIR}/audacity.qrc\"\
      )"
   )

   if (XCODE_VERSION VERSION_GREATER_EQUAL 14 AND NOT ${_OPT}perform_codesign)
      install(CODE
         "execute_process(COMMAND codesign --force --deep --sign - \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/Audacity.app\")"
      )
   endif()
else()
   install(
      TARGETS ${QAUDACITY}
      RUNTIME
      RESOURCE DESTINATION "${_PKGDATA}")

   configure_file( "${CMAKE_SOURCE_DIR}/src/audacity.desktop.in" "${CMAKE_BINARY_DIR}/audacity.desktop" )

   install( FILES "${CMAKE_BINARY_DIR}/audacity.desktop"
            DESTINATION "${_DATADIR}/applications" )
   install( FILES "${CMAKE_SOURCE_DIR}/LICENSE.txt" "${CMAKE_SOURCE_DIR}/README.md"
            DESTINATION "${_DATADIR}/doc/${AUDACITY_NAME}" )
   install( FILES "${CMAKE_SOURCE_DIR}/src/audacity.xml"
            DESTINATION "${_DATADIR}/mime/packages" )

   install(CODE
      "execute_process(\
         COMMAND \"${PYTHON}\" \"${CMAKE_SOURCE_DIR}/scripts/build/deploy_linux.py\"\
         --executable-name \"$<TARGET_FILE_NAME:${QAUDACITY}>\"\
         --source \"$<TARGET_FILE_DIR:${QAUDACITY}>/..\"\
         --dest \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}\"\
         --qrc-files \"${CMAKE_CURRENT_SOURCE_DIR}/audacity.qrc\"\
         --lib-dir \"${_PKGLIB}\"\
      )"
   )

   add_executable(findlib "${CMAKE_SOURCE_DIR}/linux/findlib.c")
   target_link_libraries(findlib ${CMAKE_DL_LIBS})
   set_target_property_all( findlib SKIP_BUILD_RPATH On )
   set_target_property_all( findlib RUNTIME_OUTPUT_DIRECTORY "${_DESTDIR}/${_EXEDIR}" )
   install(TARGETS findlib RUNTIME)

endif()
