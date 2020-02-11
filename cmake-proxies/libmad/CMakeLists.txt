
add_library( ${TARGET} STATIC )

def_vars()

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/bit.c
      ${TARGET_ROOT}/decoder.c
      ${TARGET_ROOT}/fixed.c
      ${TARGET_ROOT}/frame.c
      ${TARGET_ROOT}/huffman.c
      ${TARGET_ROOT}/layer12.c
      ${TARGET_ROOT}/layer3.c
      ${TARGET_ROOT}/stream.c
      ${TARGET_ROOT}/synth.c
      ${TARGET_ROOT}/timer.c
      ${TARGET_ROOT}/version.c
)

list( APPEND INCLUDES
   PRIVATE
      ${_PRVDIR}
   PUBLIC
      ${_PUBDIR}
      ${TARGET_ROOT}
)

list( APPEND DEFINES
   PRIVATE
      HAVE_CONFIG_H
      OPT_ACCURACY
      $<$<IN_LIST:${CMAKE_HOST_SYSTEM_PROCESSOR},X86;AMD64;i386;i686;x86_64>:FPM_INTEL>
      $<$<NOT:$<IN_LIST:${CMAKE_HOST_SYSTEM_PROCESSOR},X86;AMD64;i386;i686;x86_64>>:FPM_DEFAULT>
)

list( APPEND OPTIONS
   PRIVATE
      $<$<C_COMPILER_ID:AppleClang,Clang,GNU>:-Wall>
)

set( FPM FPM_DEFAULT )

configure_file( config.h.in private/config.h )
configure_file( mad.h.in public/mad.h )

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

