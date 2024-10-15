if( CPACK_SOURCE_INSTALLED_DIRECTORIES )
  set( package_directory "${CPACK_PACKAGE_DIRECTORY}/_CPack_Packages/${CPACK_SOURCE_TOPLEVEL_TAG}/${CPACK_SOURCE_GENERATOR}/${CPACK_SOURCE_PACKAGE_FILE_NAME}" )
  
  file(
    COPY "${CPACK_AUDACITY_BUILD_DIR}/Variables.cmake"
    DESTINATION "${package_directory}/cmake-proxies/cmake-modules"
  )
endif()
