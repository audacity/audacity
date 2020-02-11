
add_library( ${TARGET} STATIC )

def_vars()

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/libtwolame/ath.c
      ${TARGET_ROOT}/libtwolame/availbits.c
      ${TARGET_ROOT}/libtwolame/bitbuffer.c
      ${TARGET_ROOT}/libtwolame/crc.c
      ${TARGET_ROOT}/libtwolame/dab.c
      ${TARGET_ROOT}/libtwolame/encode.c
      ${TARGET_ROOT}/libtwolame/energy.c
      ${TARGET_ROOT}/libtwolame/fft.c
      ${TARGET_ROOT}/libtwolame/get_set.c
      ${TARGET_ROOT}/libtwolame/mem.c
      ${TARGET_ROOT}/libtwolame/psycho_0.c
      ${TARGET_ROOT}/libtwolame/psycho_1.c
      ${TARGET_ROOT}/libtwolame/psycho_2.c
      ${TARGET_ROOT}/libtwolame/psycho_3.c
      ${TARGET_ROOT}/libtwolame/psycho_4.c
      ${TARGET_ROOT}/libtwolame/psycho_n1.c
      ${TARGET_ROOT}/libtwolame/subband.c
      ${TARGET_ROOT}/libtwolame/twolame.c
      ${TARGET_ROOT}/libtwolame/util.c
)

list( APPEND INCLUDES
   PRIVATE
      ${_PRVDIR}
   PUBLIC
      ${TARGET_ROOT}/libtwolame
)

list( APPEND DEFINES
   PRIVATE
      LIBTWOLAME_STATIC
)

list( APPEND OPTIONS
   PRIVATE
      $<$<C_COMPILER_ID:AppleClang,Clang,GNU>:-Wno-implicit-function-declaration>
)

set( PACKAGE_BUGREPORT "twolame-discuss@lists.sourceforge.net" )
set( PACKAGE_VERSION "0.3.13" )

configure_file( config.h.in private/config.h )

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )

