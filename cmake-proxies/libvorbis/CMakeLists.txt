
add_library( ${TARGET} STATIC )

def_vars()

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/lib/analysis.c
      ${TARGET_ROOT}/lib/bitrate.c
      ${TARGET_ROOT}/lib/block.c
      ${TARGET_ROOT}/lib/codebook.c
      ${TARGET_ROOT}/lib/envelope.c
      ${TARGET_ROOT}/lib/floor0.c
      ${TARGET_ROOT}/lib/floor1.c
      ${TARGET_ROOT}/lib/info.c
      ${TARGET_ROOT}/lib/lookup.c
      ${TARGET_ROOT}/lib/lpc.c
      ${TARGET_ROOT}/lib/lsp.c
      ${TARGET_ROOT}/lib/mapping0.c
      ${TARGET_ROOT}/lib/mdct.c
      ${TARGET_ROOT}/lib/psy.c
      ${TARGET_ROOT}/lib/registry.c
      ${TARGET_ROOT}/lib/res0.c
      ${TARGET_ROOT}/lib/sharedbook.c
      ${TARGET_ROOT}/lib/smallft.c
      ${TARGET_ROOT}/lib/synthesis.c
      ${TARGET_ROOT}/lib/vorbisenc.c
      ${TARGET_ROOT}/lib/vorbisfile.c
      ${TARGET_ROOT}/lib/window.c
)

list( APPEND INCLUDES
   PRIVATE
      ${_PRVDIR}
      ${TARGET_ROOT}/lib
   PUBLIC
      ${TARGET_ROOT}/include
)

list( APPEND DEFINES
   PRIVATE
      HAVE_CONFIG_H
)

list( APPEND LIBRARIES
   PRIVATE
      libogg
)

configure_file( config.h.in private/config.h )

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

