#directory cmake-proxies/libscorealign
set( TARGET libscorealign )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
#${LIB_SRC_DIRECTORY}libscorealign/alignfiles.cpp
#${LIB_SRC_DIRECTORY}libscorealign/audiofilereader-snd.cpp
#${LIB_SRC_DIRECTORY}libscorealign/audiofilereader.cpp

${LIB_SRC_DIRECTORY}libscorealign/audioreader.cpp
${LIB_SRC_DIRECTORY}libscorealign/comp_chroma.cpp
${LIB_SRC_DIRECTORY}libscorealign/curvefit.cpp
${LIB_SRC_DIRECTORY}libscorealign/gen_chroma.cpp
${LIB_SRC_DIRECTORY}libscorealign/hillclimb.cpp
#${LIB_SRC_DIRECTORY}libscorealign/main.cpp
${LIB_SRC_DIRECTORY}libscorealign/regression.cpp
${LIB_SRC_DIRECTORY}libscorealign/sautils.cpp
${LIB_SRC_DIRECTORY}libscorealign/scorealign.cpp
#${LIB_SRC_DIRECTORY}libscorealign/trace.cpp
#${LIB_SRC_DIRECTORY}libscorealign/compare_transcripts/compare.cpp
${LIB_SRC_DIRECTORY}libscorealign/fft3/FFT3.cpp
)
# This defines the #define on both Windows and Linux.
add_definitions( 
-D_LIB
)
add_library( ${TARGET} STATIC ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 
${LIB_SRC_DIRECTORY}/portsmf
)

target_link_libraries( ${TARGET} )