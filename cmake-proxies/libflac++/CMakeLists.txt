#directory cmake-proxies/libflac++
#UNUSED
set( TARGET libflac++ )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}libflac )
project( ${TARGET} )

set( SOURCES 
${LIB_SRC_DIRECTORY}libflac/src/libFLAC++/metadata.cpp
${LIB_SRC_DIRECTORY}libflac/src/libFLAC++/stream_decoder.cpp
${LIB_SRC_DIRECTORY}libflac/src/libFLAC++/stream_encoder.cpp
#${LIB_SRC_DIRECTORY}libflac/src/test_libFLAC++/decoders.cpp
#${LIB_SRC_DIRECTORY}libflac/src/test_libFLAC++/encoders.cpp
#${LIB_SRC_DIRECTORY}libflac/src/test_libFLAC++/main.cpp
#${LIB_SRC_DIRECTORY}libflac/src/test_libFLAC++/metadata.cpp
#${LIB_SRC_DIRECTORY}libflac/src/test_libFLAC++/metadata_manip.cpp
#${LIB_SRC_DIRECTORY}libflac/src/test_libFLAC++/metadata_object.cpp
)
# This defines the #define on both Windows and Linux.
add_definitions( 
-DFLAC__NO_DLL
-D_LIB
 )

add_library( ${TARGET} STATIC ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 
${TARGET_SOURCE}/include
)

target_link_libraries( ${TARGET} )