#directory cmake-proxies/sbsms
set( TARGET sbsms )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
${LIB_SRC_DIRECTORY}sbsms/src/buffer.cpp
${LIB_SRC_DIRECTORY}sbsms/src/dBTable.cpp
${LIB_SRC_DIRECTORY}sbsms/src/fft.cpp
${LIB_SRC_DIRECTORY}sbsms/src/grain.cpp
${LIB_SRC_DIRECTORY}sbsms/src/resample.cpp
${LIB_SRC_DIRECTORY}sbsms/src/sbsms.cpp
${LIB_SRC_DIRECTORY}sbsms/src/slide.cpp
${LIB_SRC_DIRECTORY}sbsms/src/sms.cpp
${LIB_SRC_DIRECTORY}sbsms/src/subband.cpp
${LIB_SRC_DIRECTORY}sbsms/src/track.cpp
${LIB_SRC_DIRECTORY}sbsms/src/trackpoint.cpp
)
# This defines the #define on both Windows and Linux.
add_definitions( 
-D_LIB
-DNDEBUG
 )
add_library( ${TARGET} STATIC ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 
${TARGET_SOURCE}/include
${TARGET_SOURCE}/win
)

target_link_libraries( ${TARGET} )