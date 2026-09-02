cmake_minimum_required( VERSION 3.15 )
project( CheckPaWinMME_GetStreamInputHandle )

set( MODULE_DIR "${CMAKE_BINARY_DIR}/../../.." )

set( CMAKE_PREFIX_PATH
   "${MODULE_DIR}/generators"
   ${CMAKE_PREFIX_PATH}
)

find_package( PortAudio REQUIRED )

add_executable( CheckPaWinMME_GetStreamInputHandle check.c )
target_link_libraries( CheckPaWinMME_GetStreamInputHandle portaudio::portaudio )
