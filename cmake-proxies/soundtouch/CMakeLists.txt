#directory cmake-proxies/soundtouch
set( TARGET soundtouch )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 

#${LIB_SRC_DIRECTORY}soundtouch/source/Android-lib/jni/soundtouch-jni.cpp
#${LIB_SRC_DIRECTORY}soundtouch/source/SoundStretch/main.cpp
#${LIB_SRC_DIRECTORY}soundtouch/source/SoundStretch/RunParameters.cpp
#${LIB_SRC_DIRECTORY}soundtouch/source/SoundStretch/WavFile.cpp
${LIB_SRC_DIRECTORY}soundtouch/source/SoundTouch/AAFilter.cpp
#${LIB_SRC_DIRECTORY}soundtouch/source/SoundTouch/BPMDetect.cpp
${LIB_SRC_DIRECTORY}soundtouch/source/SoundTouch/cpu_detect_x86.cpp
${LIB_SRC_DIRECTORY}soundtouch/source/SoundTouch/FIFOSampleBuffer.cpp
${LIB_SRC_DIRECTORY}soundtouch/source/SoundTouch/FIRFilter.cpp
${LIB_SRC_DIRECTORY}soundtouch/source/SoundTouch/mmx_optimized.cpp
#${LIB_SRC_DIRECTORY}soundtouch/source/SoundTouch/PeakFinder.cpp
${LIB_SRC_DIRECTORY}soundtouch/source/SoundTouch/RateTransposer.cpp
${LIB_SRC_DIRECTORY}soundtouch/source/SoundTouch/SoundTouch.cpp
${LIB_SRC_DIRECTORY}soundtouch/source/SoundTouch/sse_optimized.cpp
${LIB_SRC_DIRECTORY}soundtouch/source/SoundTouch/TDStretch.cpp
#${LIB_SRC_DIRECTORY}soundtouch/source/SoundTouchDLL/SoundTouchDLL.cpp
)
# This defines the #define on both Windows and Linux.
add_definitions( 
-D_LIB
 )
add_library( ${TARGET} STATIC ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 
${TARGET_SOURCE}/include
)

target_link_libraries( ${TARGET} )