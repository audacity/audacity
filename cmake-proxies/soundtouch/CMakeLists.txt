
add_library( ${TARGET} STATIC )

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
      ${CMAKE_CURRENT_BINARY_DIR}/public
      ${TARGET_ROOT}/include
)

  
if( UNIX )
   check_cxx_compiler_flag( "-mmmx" HAVE_MMX )
   if( HAVE_MMX )
      list( APPEND OPTIONS
         PRIVATE
            -mmmx
      )
   endif()

   check_cxx_compiler_flag( "-msse" HAVE_SSE )
   if( HAVE_SSE )
      list( APPEND OPTIONS
         PRIVATE
            -msse
      )
   endif()
endif()

configure_file( soundtouch_config.h.in public/soundtouch_config.h )

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_features( ${TARGET} PRIVATE ${FEATURES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

