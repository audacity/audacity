
add_library( ${TARGET} STATIC )

def_vars()

set(CMAKE_MODULE_PATH ${TARGET_ROOT}/cmake/Modules )

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/src/cr.c
      ${TARGET_ROOT}/src/cr32.c
      ${TARGET_ROOT}/src/cr32s.c
      ${TARGET_ROOT}/src/cr64.c
      ${TARGET_ROOT}/src/data-io.c
      ${TARGET_ROOT}/src/dbesi0.c
      ${TARGET_ROOT}/src/fft4g.c
      ${TARGET_ROOT}/src/fft4g32.c
      ${TARGET_ROOT}/src/fft4g64.c
      ${TARGET_ROOT}/src/filter.c
      ${TARGET_ROOT}/src/pffft32s.c
      ${TARGET_ROOT}/src/soxr.c
      ${TARGET_ROOT}/src/util32s.c
      ${TARGET_ROOT}/src/vr32.c
)

list( APPEND INCLUDES
   PRIVATE
      ${_PRVDIR}
   PUBLIC
      ${TARGET_ROOT}/src
)

list( APPEND DEFINES
   PRIVATE
      _USE_MATH_DEFINES
      _CRT_SECURE_NO_WARNINGS
      SOXR_LIB
)

list( APPEND OPTIONS
   PRIVATE
      $<$<C_COMPILER_ID:AppleClang,Clang,GNU>:
         -Wconversion
         -Wall
         -Wextra
         -pedantic
         -Wundef
         -Wpointer-arith
         -Wno-long-long
         -Wnested-externs
         -Wmissing-prototypes
         -Wstrict-prototypes
      >
      $<$<C_COMPILER_ID:AppleClang,Clang>:-Wno-keyword-macro>
)

set( HAVE_BIGENDIAN ${WORDS_BIGENDIAN} )

set( AVCODEC_FOUND NO )
set( AVUTIL_FOUND NO )
set( WITH_PFFFT YES )

set( WITH_CR32 YES )
set( WITH_CR32S NO )
set( WITH_CR64 YES )
set( WITH_CR64S NO )
set( WITH_VR32 YES )

find_package( SIMD32 )
if( SIMD32_FOUND )
   set( WITH_CR32S YES )
   string( STRIP "${SIMD32_C_FLAGS}" SIMD32_C_FLAGS )
   string( REPLACE " " ";" SIMD32_C_FLAGS "${SIMD32_C_FLAGS}" )
   list( APPEND OPTIONS ${SIMD32_C_FLAGS} )
endif()

# Disabling since we do not want AVX instructions in distribution builds
#
#find_package( SIMD64 )
#if( SIMD64_FOUND )
#   set( WITH_CR64S ON )
#   string( STRIP ${SIMD64_C_FLAGS} SIMD64_C_FLAGS )
#   list( APPEND OPTIONS ${SIMD64_C_FLAGS} )
#endif()

set( WITH_HI_PREC_CLOCK YES )
set( WITH_FLOAT_STD_PREC_CLOCK NO )
set( WITH_DEV_TRACE NO )

configure_file( soxr-config.h.in private/soxr-config.h )

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )

if( CMAKE_CXX_COMPILER_ID STREQUAL "GNU" )
   target_compile_options( ${TARGET} PRIVATE -fPIC )
endif()
