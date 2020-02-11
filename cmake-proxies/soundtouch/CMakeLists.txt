
add_library( ${TARGET} STATIC )

def_vars()

list( APPEND SOURCES 
   PRIVATE
      ${TARGET_ROOT}/source/SoundTouch/AAFilter.cpp
      ${TARGET_ROOT}/source/SoundTouch/FIFOSampleBuffer.cpp
      ${TARGET_ROOT}/source/SoundTouch/FIRFilter.cpp
      ${TARGET_ROOT}/source/SoundTouch/mmx_optimized.cpp
      ${TARGET_ROOT}/source/SoundTouch/RateTransposer.cpp
      ${TARGET_ROOT}/source/SoundTouch/SoundTouch.cpp
      ${TARGET_ROOT}/source/SoundTouch/TDStretch.cpp
      ${TARGET_ROOT}/source/SoundTouch/cpu_detect_x86.cpp
      ${TARGET_ROOT}/source/SoundTouch/sse_optimized.cpp
)

list( APPEND INCLUDES
   PUBLIC
      ${_PUBDIR}
      ${TARGET_ROOT}/include
)

list( APPEND OPTIONS
   PRIVATE
      ${MMX_FLAG}
      ${SSE_FLAG}
)

configure_file( soundtouch_config.h.in public/soundtouch_config.h )

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )

