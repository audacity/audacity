set(TARGET soundtouch)
add_library( ${TARGET} STATIC )

set(TARGET_ROOT ${AUDACITY_ROOT}/lib-src/soundtouch)
list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/source/SoundTouch/AAFilter.cpp
      ${TARGET_ROOT}/source/SoundTouch/AAFilter.h
      ${TARGET_ROOT}/source/SoundTouch/BPMDetect.cpp
      ${TARGET_ROOT}/source/SoundTouch/cpu_detect.h
      ${TARGET_ROOT}/source/SoundTouch/cpu_detect_x86.cpp
      ${TARGET_ROOT}/source/SoundTouch/FIFOSampleBuffer.cpp
      ${TARGET_ROOT}/source/SoundTouch/FIRFilter.cpp
      ${TARGET_ROOT}/source/SoundTouch/FIRFilter.h
      ${TARGET_ROOT}/source/SoundTouch/PeakFinder.cpp
      ${TARGET_ROOT}/source/SoundTouch/PeakFinder.h
      ${TARGET_ROOT}/source/SoundTouch/RateTransposer.cpp
      ${TARGET_ROOT}/source/SoundTouch/RateTransposer.h
      ${TARGET_ROOT}/source/SoundTouch/SoundTouch.cpp
      ${TARGET_ROOT}/source/SoundTouch/TDStretch.cpp
      ${TARGET_ROOT}/source/SoundTouch/TDStretch.h
      ${TARGET_ROOT}/source/SoundTouch/mmx_optimized.cpp
      ${TARGET_ROOT}/source/SoundTouch/sse_optimized.cpp
)

list( APPEND INCLUDES
   PUBLIC
      ${TARGET_ROOT}/include
   PRIVATE
      ${CMAKE_CURRENT_BINARY_DIR}/private
)

list( APPEND DEFINES
   PUBLIC
      USE_SOUNDTOUCH=1
      # Define sample type as float (not integer)
      # Note: We only define SOUNDTOUCH_FLOAT_SAMPLES, not SOUNDTOUCH_INTEGER_SAMPLES
      # because STTypes.h uses #ifdef which checks if the macro is defined, not its value
      SOUNDTOUCH_FLOAT_SAMPLES=1
   PRIVATE
      # Workaround for bug in AU3's BPMDetect.cpp line 133:
      # It uses INPUT_BLOCK_SIZE and DECIMATED_BLOCK_SIZE but the file defines
      # INPUT_BLOCK_SAMPLES and DECIMATED_BLOCK_SAMPLES
      INPUT_BLOCK_SIZE=2048
      DECIMATED_BLOCK_SIZE=256
)

# Check for headers
include(CheckIncludeFile)
check_include_file(cpuid.h HAVE_CPUID_H)
check_include_file(inttypes.h HAVE_INTTYPES_H)
check_include_file(stdint.h HAVE_STDINT_H)
check_include_file(stdlib.h HAVE_STDLIB_H)
check_include_file(string.h HAVE_STRING_H)
check_include_file(unistd.h HAVE_UNISTD_H)

list( APPEND OPTIONS
   PRIVATE
      $<$<CXX_COMPILER_ID:AppleClang,Clang,GNU>:
         -O3
         -fcheck-new
         -Wno-unused-parameter
         -Wno-unused-variable
         -Wno-unused-function
      >
      $<$<CXX_COMPILER_ID:MSVC>:
         /O2
         /wd4100
         /wd4505
      >
)

list( APPEND PROPERTIES
   POSITION_INDEPENDENT_CODE On
)

configure_file( soundtouch_config.h.in private/soundtouch_config.h )

target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
set_target_properties( ${TARGET} PROPERTIES ${PROPERTIES} )

