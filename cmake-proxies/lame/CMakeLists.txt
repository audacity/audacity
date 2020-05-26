
add_library( ${TARGET} STATIC )

def_vars()

list( APPEND SOURCES
   PRIVATE
      # libmp3lame
      ${TARGET_ROOT}/libmp3lame/bitstream.c
      ${TARGET_ROOT}/libmp3lame/encoder.c
      ${TARGET_ROOT}/libmp3lame/fft.c
      ${TARGET_ROOT}/libmp3lame/gain_analysis.c
      ${TARGET_ROOT}/libmp3lame/id3tag.c
      ${TARGET_ROOT}/libmp3lame/lame.c
      ${TARGET_ROOT}/libmp3lame/newmdct.c
      ${TARGET_ROOT}/libmp3lame/presets.c
      ${TARGET_ROOT}/libmp3lame/psymodel.c
      ${TARGET_ROOT}/libmp3lame/quantize.c
      ${TARGET_ROOT}/libmp3lame/quantize_pvt.c
      ${TARGET_ROOT}/libmp3lame/reservoir.c
      ${TARGET_ROOT}/libmp3lame/set_get.c
      ${TARGET_ROOT}/libmp3lame/tables.c
      ${TARGET_ROOT}/libmp3lame/takehiro.c
      ${TARGET_ROOT}/libmp3lame/util.c
      ${TARGET_ROOT}/libmp3lame/vbrquantize.c
      ${TARGET_ROOT}/libmp3lame/VbrTag.c
      ${TARGET_ROOT}/libmp3lame/version.c
      ${TARGET_ROOT}/libmp3lame/vector/xmm_quantize_sub.c
)

list( APPEND INCLUDES
   PRIVATE
      ${_PRVDIR}
      ${TARGET_ROOT}/lame
      ${TARGET_ROOT}/libmp3lame
   PUBLIC
      ${TARGET_ROOT}
)

list( APPEND DEFINES
   PRIVATE
      HAVE_CONFIG_H
)

list( APPEND OPTIONS
   PRIVATE
      ${MMX_FLAG}
      ${SSE_FLAG}
)

set( HAVE_INT8_T SIZEOF_INT8_T )
set( HAVE_INT16_T SIZEOF_INT16_T )
set( HAVE_INT32_T SIZEOF_INT32_T )
set( HAVE_INT64_T SIZEOF_INT64_T )
set( HAVE_UINT16_T SIZEOF_UINT16_T )
set( HAVE_UINT32_T SIZEOF_UINT32_T )
set( HAVE_UINT64_T SIZEOF_UINT64_T )

if( SIZEOF_SHORT EQUAL 4 )
   set( A_INT32_T int )
elseif( SIZEOF_INT EQUAL 4 )
   set( A_INT32_T int )
elseif( SIZEOF_LONG EQUAL 4 )
   set( A_INT32_T long )
endif()

if( SIZEOF_INT EQUAL 8 )
   set( A_INT64_T int )
elseif( SIZEOF_LONG EQUAL 8 )
   set( A_INT64_T long )
elseif( SIZEOF_LONG_LONG EQUAL 8 )
   set( A_INT64_T long long )
endif()

set( TAKEHIRO_IEEE754_HACK 1 )
set( USE_FAST_LOG 1 )
if( ${CMakeDetermineASM_NASMCompiler} )
   set( HAVE_NASM 1 )
endif()

configure_file( config.h.in private/config.h )

#organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

