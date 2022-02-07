if(CPACK_EXTERNAL_ENABLE_STAGING)
   set(appdir "${CPACK_TEMPORARY_DIRECTORY}")
else()
   set(appdir "${CPACK_INSTALL_PREFIX}")
endif()
set(appimage "${CPACK_PACKAGE_DIRECTORY}/${CPACK_PACKAGE_FILE_NAME}.AppImage")

set(CPACK_EXTERNAL_BUILT_PACKAGES "${appimage}")

if(DEFINED CPACK_AUDACITY_APPIMAGE_UPDATE_INFO)
   set(ENV{AUDACITY_UPDATE_INFO} "${CPACK_AUDACITY_APPIMAGE_UPDATE_INFO}")
   list(APPEND CPACK_EXTERNAL_BUILT_PACKAGES "${appimage}.zsync")
endif()

configure_file("${CPACK_AUDACITY_SOURCE_DIR}/linux/AppRun.sh" "${appdir}/AppRun" ESCAPE_QUOTES @ONLY)
configure_file("${CPACK_AUDACITY_SOURCE_DIR}/linux/check_dependencies.sh" "${appdir}/bin/check_dependencies" ESCAPE_QUOTES @ONLY)
configure_file("${CPACK_AUDACITY_SOURCE_DIR}/linux/ldd_recursive.pl" "${appdir}/bin/ldd_recursive" COPYONLY)
file(COPY "${CPACK_AUDACITY_FINDLIB_LOCATION}/findlib" DESTINATION "${appdir}/bin/")

execute_process(
   COMMAND "${CPACK_AUDACITY_SOURCE_DIR}/linux/create_appimage.sh" "${appdir}" "${appimage}"
   WORKING_DIRECTORY "${CPACK_AUDACITY_BUILD_DIR}"
   RESULT_VARIABLE exit_status
)

if(NOT "${exit_status}" EQUAL "0")
   message(FATAL_ERROR "Could not create AppImage. See output above for details.")
endif()
