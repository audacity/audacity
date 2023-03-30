qt_standard_project_setup()
set(QAUDACITY ${CMAKE_PROJECT_NAME})

set(QT_QML_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/qml)

set(SOURCES
   main.cpp
   EditToolbarHandler.cpp
   EditToolbarHandler.h
   ToolsToolbarHandler.cpp
   ToolsToolbarHandler.h
   TransportToolbarHandler.cpp
   TransportToolbarHandler.h
)

set(RESOURCES
   audacity.qrc
   fonts/fonts.qrc
)

qt_add_resources(QT_RESOURCES
   ${RESOURCES}
)

add_subdirectory(uicomponents)

qt_add_executable(${QAUDACITY} ${SOURCES} ${QT_RESOURCES} MANUAL_FINALIZATION)

set(QML_SOURCES
   qml/main.qml
   qml/EditToolbar.qml
   qml/MasterVolumeToolbar.qml
   qml/Sidebar.qml
   qml/TimeToolbar.qml
   qml/ToolsToolbar.qml
   qml/TransportToolbar.qml
   qml/Workspace.qml
)

qt_add_qml_module(
	${QAUDACITY}
	URI Audacity
	VERSION 1.0
   DEPENDENCIES
      QtQuick
      Audacity.UiComponents
	QML_FILES
		${QML_SOURCES}
   NO_LINT
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

target_compile_definitions( ${QAUDACITY} PRIVATE $<$<CONFIG:Debug>:QT_QML_DEBUG> )
target_link_libraries( ${QAUDACITY} PUBLIC ${AUDACITY_LIBRARIES} )
target_link_libraries( ${QAUDACITY} PRIVATE Qt6::Core Qt6::Gui Qt6::Quick kddockwidgets::kddockwidgets uicomponentsplugin)

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
