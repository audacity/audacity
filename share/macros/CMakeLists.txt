#
# Audacity: A Digital Audio Editor
#

set(TARGET builtin-macros)

set(MACRO_FILES
    FadeInStart.json
    MP3Conversion.json
)

set(MACROS_INSTALL_LOCATION "${MUSE_APP_INSTALL_RESOURCES_LOCATION}macros")

if(OS_IS_MAC)
    set(MACROS_BUILD_DEST "${CMAKE_BINARY_DIR}/src/app/${MACROS_INSTALL_LOCATION}")
elseif(OS_IS_LIN)
    set(MACROS_BUILD_DEST "${CMAKE_BINARY_DIR}/src/app/${MACROS_INSTALL_LOCATION}")
else()
    set(MACROS_BUILD_DEST "${CMAKE_BINARY_DIR}/${MACROS_INSTALL_LOCATION}")
endif()

foreach(source ${MACRO_FILES})
    set(src "${CMAKE_CURRENT_SOURCE_DIR}/${source}")
    set(dst "${MACROS_BUILD_DEST}/${source}")

    add_custom_command(
        DEPENDS "${src}"
        COMMAND ${CMAKE_COMMAND} -E make_directory "${MACROS_BUILD_DEST}"
        COMMAND ${CMAKE_COMMAND} -E copy "${src}" "${dst}"
        OUTPUT "${dst}"
    )

    list(APPEND SOURCES "${src}")
    list(APPEND OUTPUTS "${dst}")
endforeach()

add_custom_target(${TARGET} ALL DEPENDS ${OUTPUTS} SOURCES ${SOURCES})

install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/
        DESTINATION ${MACROS_INSTALL_LOCATION}
        FILES_MATCHING PATTERN "*.json"
        PATTERN "CMakeLists.txt" EXCLUDE
        PATTERN ".*" EXCLUDE
)
