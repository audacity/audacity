
add_library( ${TARGET} STATIC )

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/compat.c
      ${TARGET_ROOT}/crc.c
      $<$<STREQUAL:${CMAKE_BUILD_TYPE},Debug>:${TARGET_ROOT}/debug.c>
      ${TARGET_ROOT}/field.c
      ${TARGET_ROOT}/file.c
      ${TARGET_ROOT}/frame.c
      ${TARGET_ROOT}/frametype.c
      ${TARGET_ROOT}/genre.c
      ${TARGET_ROOT}/latin1.c
      ${TARGET_ROOT}/parse.c
      ${TARGET_ROOT}/render.c
      ${TARGET_ROOT}/tag.c
      ${TARGET_ROOT}/ucs4.c
      ${TARGET_ROOT}/utf16.c
      ${TARGET_ROOT}/utf8.c
      ${TARGET_ROOT}/util.c
      ${TARGET_ROOT}/version.c
)

list( APPEND INCLUDES
   PRIVATE
      ${CMAKE_CURRENT_BINARY_DIR}/private
   PUBLIC
      ${TARGET_ROOT}
)

list( APPEND OPTIONS
   PRIVATE
      $<$<C_COMPILER_ID:GNU>:-Wno-implicit-function-declaration>
)

list( APPEND LIBRARIES
   PRIVATE
     wxwidgets
)
configure_file( config.h.in private/config.h )

organize_source( "${TARGET_ROOT}" "src" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_features( ${TARGET} PRIVATE ${FEATURES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

