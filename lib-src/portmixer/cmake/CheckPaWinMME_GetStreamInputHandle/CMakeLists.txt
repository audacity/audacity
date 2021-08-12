cmake_minimum_required( VERSION 3.15 )
project( CheckPaWinMME_GetStreamInputHandle )

set( MODULE_DIR "${CMAKE_BINARY_DIR}/../../.." )

set( CMAKE_MODULE_PATH 
    ${MODULE_DIR}
    ${CMAKE_MODULE_PATH}
)

set( PortAudio_DIR ${MODULE_DIR} )

find_package( PortAudio REQUIRED )

add_executable( CheckPaWinMME_GetStreamInputHandle check.c )
target_link_libraries( CheckPaWinMME_GetStreamInputHandle PortAudio::PortAudio )
