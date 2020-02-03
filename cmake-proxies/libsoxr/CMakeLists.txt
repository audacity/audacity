
add_library( ${TARGET} STATIC )

set(CMAKE_MODULE_PATH ${TARGET_ROOT}/cmake/Modules )

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/src/cr.c
      ${TARGET_ROOT}/src/cr32.c
      ${TARGET_ROOT}/src/cr32s.c
      ${TARGET_ROOT}/src/cr64.c
      ${TARGET_ROOT}/src/cr64s.c
      ${TARGET_ROOT}/src/data-io.c
      ${TARGET_ROOT}/src/dbesi0.c
      ${TARGET_ROOT}/src/fft4g.c
      ${TARGET_ROOT}/src/fft4g32.c
      ${TARGET_ROOT}/src/fft4g64.c
      ${TARGET_ROOT}/src/filter.c
      ${TARGET_ROOT}/src/pffft32s.c
      ${TARGET_ROOT}/src/pffft64s.c
      ${TARGET_ROOT}/src/soxr.c
      ${TARGET_ROOT}/src/util32s.c
      ${TARGET_ROOT}/src/util64s.c
      ${TARGET_ROOT}/src/vr32.c
)

list( APPEND INCLUDES
   PRIVATE
      ${CMAKE_CURRENT_BINARY_DIR}/private
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
      $<$<IN_LIST:${CMAKE_C_COMPILER_ID},GNU;Clang;AppleClang>:-Wall>
)

if( CMAKE_C_COMPILER_ID MATCHES "GNU|.*Clang" )
   list( APPEND OPTIONS
      PRIVATE
         -Wconversion -Wall -Wextra -pedantic -Wundef -Wpointer-arith -Wno-long-long
         $<$<C_COMPILER_ID:Clang>:-Wno-keyword-macro>
         $<$<COMPILE_LANGUAGE:CXX>:
            -std=gnu89 -Wnested-externs -Wmissing-prototypes -Wstrict-prototypes
         >
   )
endif()

set( HAVE_BIGENDIAN ${WORDS_BIGENDIAN} )

set( AVCODEC_FOUND NO )
set( AVUTIL_FOUND NO )
set( WITH_PFFFT YES )

set( WITH_CR32 YES )
set( WITH_CR32S YES )
set( WITH_CR64 YES )
set( WITH_CR64S YES )
set( WITH_VR32 YES )

# Copied from libsoxr CMakeLists.txt
if( WITH_CR32S )
   find_package( SIMD32 )
   set( WITH_CR32S ${SIMD32_FOUND} )
#   list( APPEND OPTIONS ${SIMD32_C_FLAGS} )
endif()

if( WITH_CR64S )
   find_package( SIMD64 )
   set( WITH_CR64S ${SIMD64_FOUND} )
   list( APPEND OPTIONS ${SIMD64_C_FLAGS} )
endif()

set( WITH_HI_PREC_CLOCK YES )
set( WITH_FLOAT_STD_PREC_CLOCK NO )
set( WITH_DEV_TRACE NO )

configure_file( soxr-config.h.in private/soxr-config.h )

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_features( ${TARGET} PRIVATE ${FEATURES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

