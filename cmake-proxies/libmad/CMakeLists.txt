#directory cmake-proxies/libmad
set( TARGET libmad )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
${LIB_SRC_DIRECTORY}libmad/bit.c
${LIB_SRC_DIRECTORY}libmad/decoder.c
${LIB_SRC_DIRECTORY}libmad/fixed.c
${LIB_SRC_DIRECTORY}libmad/frame.c
${LIB_SRC_DIRECTORY}libmad/huffman.c
${LIB_SRC_DIRECTORY}libmad/layer12.c
${LIB_SRC_DIRECTORY}libmad/layer3.c
${LIB_SRC_DIRECTORY}libmad/minimad.c
${LIB_SRC_DIRECTORY}libmad/stream.c
${LIB_SRC_DIRECTORY}libmad/synth.c
${LIB_SRC_DIRECTORY}libmad/timer.c
${LIB_SRC_DIRECTORY}libmad/version.c
)
# This defines the #define on both Windows and Linux.
add_definitions(  )
add_library( ${TARGET} MODULE ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 

)

target_link_libraries( ${TARGET} )