cmake_minimum_required( VERSION 3.15 )
project( PaWinDS_GetDeviceGUID )

set( MODULE_DIR "${CMAKE_BINARY_DIR}/../../.." )

set( CMAKE_PREFIX_PATH
   "${MODULE_DIR}/generators"
   ${CMAKE_PREFIX_PATH}
)

find_package( PortAudio REQUIRED )

add_executable( PaWinDS_GetDeviceGUID check.c )
target_link_libraries( PaWinDS_GetDeviceGUID portaudio::portaudio )
