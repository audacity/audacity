#
# Audacity: A Digital Audio Editor
#

set(TARGET builtin-workspaces)

set(WORKSPACE_FILES
    Classic.mws
    Music.mws
    Modern.mws
)

if(OS_IS_MAC)
    set(WORKSPACES_BUILD_DEST "${CMAKE_BINARY_DIR}/src/app/${AU4_SHARE_NAME}${AU4_INSTALL_NAME}workspaces")
elseif(OS_IS_LIN)
    set(WORKSPACES_BUILD_DEST "${CMAKE_BINARY_DIR}/share/audacity/workspaces")
else()
    set(WORKSPACES_BUILD_DEST "${CMAKE_BINARY_DIR}/${AU4_SHARE_NAME}${AU4_INSTALL_NAME}workspaces")
endif()

foreach(source ${WORKSPACE_FILES})
    set(src "${CMAKE_CURRENT_SOURCE_DIR}/${source}")
    set(dst "${WORKSPACES_BUILD_DEST}/${source}")

    add_custom_command(
        DEPENDS "${src}"
        COMMAND ${CMAKE_COMMAND} -E make_directory "${WORKSPACES_BUILD_DEST}"
        COMMAND ${CMAKE_COMMAND} -E copy "${src}" "${dst}"
        OUTPUT "${dst}"
    )

    list(APPEND SOURCES "${src}")
    list(APPEND OUTPUTS "${dst}")
endforeach()

add_custom_target(${TARGET} ALL DEPENDS ${OUTPUTS} SOURCES ${SOURCES})

# Install for release builds
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/
        DESTINATION ${AU4_SHARE_NAME}${AU4_INSTALL_NAME}workspaces
        FILES_MATCHING PATTERN "*.mws"
        PATTERN "CMakeLists.txt" EXCLUDE
        PATTERN ".*" EXCLUDE
)
