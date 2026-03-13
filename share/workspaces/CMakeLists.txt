#
# Audacity: A Digital Audio Editor
#

set(TARGET workspaces)

set(WORKSPACE_FILES
    Classic.mws
    Modern.mws
    Music.mws
)

# Determine build destination based on platform
if(OS_IS_MAC)
    set(WS_BUILD_DEST "${CMAKE_BINARY_DIR}/src/app/${AU4_SHARE_NAME}${AU4_INSTALL_NAME}workspaces")
elseif(OS_IS_LIN)
    set(WS_BUILD_DEST "${CMAKE_BINARY_DIR}/share/audacity/workspaces")
else()
    set(WS_BUILD_DEST "${CMAKE_BINARY_DIR}/${AU4_SHARE_NAME}${AU4_INSTALL_NAME}/workspaces")
endif()

# Install for release builds
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/
        DESTINATION ${AU4_SHARE_NAME}${AU4_INSTALL_NAME}/workspaces
        FILES_MATCHING PATTERN "*.mws"
        PATTERN "CMakeLists.txt" EXCLUDE
        PATTERN ".*" EXCLUDE
)

# Copy to build directory for development builds
foreach(source ${WORKSPACE_FILES})
    set(src "${CMAKE_CURRENT_SOURCE_DIR}/${source}")
    set(dst "${WS_BUILD_DEST}/${source}")

    add_custom_command(
        DEPENDS "${src}"
        COMMAND ${CMAKE_COMMAND} -E make_directory "${WS_BUILD_DEST}"
        COMMAND ${CMAKE_COMMAND} -E copy "${src}" "${dst}"
        OUTPUT "${dst}"
    )

    list(APPEND SOURCES "${src}")
    list(APPEND OUTPUTS "${dst}")
endforeach()

add_custom_target(${TARGET} ALL DEPENDS ${OUTPUTS} SOURCES ${SOURCES})