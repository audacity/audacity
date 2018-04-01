#directory cmake-proxies/portsmf
set( TARGET portsmf )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
${LIB_SRC_DIRECTORY}portsmf/allegro.cpp
${LIB_SRC_DIRECTORY}portsmf/allegrord.cpp
${LIB_SRC_DIRECTORY}portsmf/allegroserial.cpp
${LIB_SRC_DIRECTORY}portsmf/allegrosmfrd.cpp
${LIB_SRC_DIRECTORY}portsmf/allegrosmfwr.cpp
${LIB_SRC_DIRECTORY}portsmf/allegrowr.cpp
${LIB_SRC_DIRECTORY}portsmf/mfmidi.cpp
${LIB_SRC_DIRECTORY}portsmf/strparse.cpp
#${LIB_SRC_DIRECTORY}portsmf/trace.cpp
#${LIB_SRC_DIRECTORY}portsmf/apps/allegroconvert.cpp
#${LIB_SRC_DIRECTORY}portsmf/apps/allegroplay.cpp
#${LIB_SRC_DIRECTORY}portsmf/apps/seq2midi.cpp
#${LIB_SRC_DIRECTORY}portsmf/portsmf_test/portsmf_test.cpp
)
# This defines the #define on both Windows and Linux.
add_definitions( 
-D_LIB
 )
add_library( ${TARGET} STATIC ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 

)

target_link_libraries( ${TARGET} )