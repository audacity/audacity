qt_standard_project_setup()
set(QAUDACITY ${CMAKE_PROJECT_NAME})

set(SOURCES
   main.cpp
)

set(RESOURCES
   audacity.qrc
)

qt_add_resources(QT_RESOURCES
   ${RESOURCES}
)

qt_add_executable(${QAUDACITY} ${SOURCES} ${QT_RESOURCES} MANUAL_FINALIZATION)

set(QML_SOURCES
   qml/main.qml
)

qt_add_qml_module(
	${QAUDACITY}
	URI qml
	VERSION 1.0
	QML_FILES
		${QML_SOURCES}

)

set_target_properties(
      ${QAUDACITY}
      PROPERTIES
         WIN32_EXECUTABLE ON
         MACOSX_BUNDLE ON
)

if(CMAKE_SYSTEM_NAME MATCHES "Darwin")
   set_target_properties(
      ${QAUDACITY}
      PROPERTIES
         RUNTIME_OUTPUT_DIRECTORY "${_DESTDIR}"
   )

   #fix_bundle(${QAUDACITY})
elseif(NOT CMAKE_SYSTEM_NAME MATCHES "Darwin|Windows")
   set_target_properties(
      ${QAUDACITY}
      PROPERTIES
         RUNTIME_OUTPUT_DIRECTORY "${_DESTDIR}/bin"
         RUNTIME_OUTPUT_NAME "audacity"
   )
endif()

target_link_libraries( ${QAUDACITY} PUBLIC ${AUDACITY_LIBRARIES} )
target_link_libraries( ${QAUDACITY} PRIVATE Qt6::Core Qt6::Gui Qt6::Quick )

qt_finalize_target(${QAUDACITY})

organize_source( "${CMAKE_CURRENT_SOURCE_DIR}" "" "${SOURCES}" )
organize_source( "${CMAKE_CURRENT_SOURCE_DIR}" "res" "${RESOURCES}" )
organize_source( "${CMAKE_CURRENT_SOURCE_DIR}" "" "${QML_SOURCES}" )

install(
   TARGETS ${QAUDACITY}
      BUNDLE  DESTINATION .
)

#if(CMAKE_SYSTEM_NAME MATCHES "Darwin|Windows")
#   qt_generate_deploy_qml_app_script(
#      TARGET ${QAUDACITY}
#      FILENAME_VARIABLE deploy_script
#      MACOS_BUNDLE_POST_BUILD
#      #NO_UNSUPPORTED_PLATFORM_ERROR
#      DEPLOY_USER_QML_MODULES_ON_UNSUPPORTED_PLATFORM
#   )

#   install(SCRIPT ${deploy_script})    # Add its runtime dependencies
#endif()
