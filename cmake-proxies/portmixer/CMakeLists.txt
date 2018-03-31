#directory cmake-proxies/portmixer
set( TARGET portmixer )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
${LIB_SRC_DIRECTORY}portmixer/px_tests/px_test.c
${LIB_SRC_DIRECTORY}portmixer/src/px_example_api.c
${LIB_SRC_DIRECTORY}portmixer/src/px_linux_alsa.c
${LIB_SRC_DIRECTORY}portmixer/src/px_mac_coreaudio.c
${LIB_SRC_DIRECTORY}portmixer/src/px_mixer.c
${LIB_SRC_DIRECTORY}portmixer/src/px_unix_oss.c
${LIB_SRC_DIRECTORY}portmixer/src/px_win_common.c
${LIB_SRC_DIRECTORY}portmixer/src/px_win_ds.c
${LIB_SRC_DIRECTORY}portmixer/src/px_win_endpoint.c
${LIB_SRC_DIRECTORY}portmixer/src/px_win_wasapi.c
${LIB_SRC_DIRECTORY}portmixer/src/px_win_wmme.c
)
# This defines the #define on both Windows and Linux.
add_definitions(  )
add_library( ${TARGET} STATIC ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 

)

target_link_libraries( ${TARGET} )