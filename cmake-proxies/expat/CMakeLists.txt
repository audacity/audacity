
add_library( ${TARGET} STATIC )

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/lib/xmlparse.c
      ${TARGET_ROOT}/lib/xmlrole.c
      ${TARGET_ROOT}/lib/xmltok.c
      ${TARGET_ROOT}/lib/xmltok_impl.c
      ${TARGET_ROOT}/lib/xmltok_ns.c
)

list( APPEND INCLUDES
   PRIVATE
      ${CMAKE_CURRENT_BINARY_DIR}/private
   PUBLIC
      ${TARGET_ROOT}/lib
)

list( APPEND DEFINES
   PRIVATE
      HAVE_EXPAT_CONFIG_H
)

check_symbol_exists( "getpagesize" "unistd.h" HAVE_GETPAGESIZE )
check_symbol_exists( "bcopy" "strings.h" HAVE_BCOPY )
check_symbol_exists( "memmove" "string.h" HAVE_MEMMOVE )

if( WORDS_BIGENDIAN )
   set( BYTEORDER 4321 )
else( WORDS_BIGENDIAN )
    set( BYTEORDER 1234 )
endif()

set( XML_DTD OFF )
set( XML_NS OFF )

configure_file( ${TARGET_ROOT}/expat_config.h.cmake private/expat_config.h )

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

