cmake_minimum_required(VERSION 2.8.11)

set(TARGET portmidi)
set(TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET})

set(CMAKE_CACHEFILE_DIR ${CMAKE_BINARY_DIR})
add_subdirectory( "${TARGET_SOURCE}" "${CMAKE_CURRENT_BINARY_DIR}/${TARGET}" EXCLUDE_FROM_ALL )
set_target_properties(portmidi-static PROPERTIES
    OSX_ARCHITECTURES ""
    OUTPUT_NAME ${TARGET})
