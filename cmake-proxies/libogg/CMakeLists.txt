
add_library( ${TARGET} STATIC )

def_vars()

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/src/bitwise.c
      ${TARGET_ROOT}/src/framing.c
)

list( APPEND INCLUDES
   PUBLIC
      ${_PUBDIR}
      ${TARGET_ROOT}/include
)

if( SIZEOF_INT16 EQUAL 2 )
    set( SIZE16 "int16_t" )
elseif( SIZEOF_SHORT EQUAL 2 )
    set( SIZE16 "short" )
elseif( SIZEOF_INT EQUAL 2 )
    set( SIZE16 "int" )
else()
    message( FATAL_ERROR "No 16 bit type found on this platform!" )
endif()

if( SIZEOF_UINT16 EQUAL 2 )
    set( USIZE16 "uint16_t" )
elseif( SIZEOF_SHORT EQUAL 2 )
    set( USIZE16 "unsigned short" )
elseif( SIZEOF_INT EQUAL 2 )
    set( USIZE16 "unsigned int" )
elseif( SIZEOF_U_INT EQUAL 2 )
    set( USIZE16 "u_int16_t" )
else()
    message( FATAL_ERROR "No unsigned 16 bit type found on this platform!" )
endif()

if( SIZEOF_INT32 EQUAL 4 )
    set( SIZE32 "int32_t" )
elseif( SIZEOF_SHORT EQUAL 4 )
    set( SIZE32 "short" )
elseif( SIZEOF_INT EQUAL 4 )
    set( SIZE32 "int" )
elseif( SIZEOF_LONG EQUAL 4 )
    set( SIZE16 "long" )
else()
    message( FATAL_ERROR "No 32 bit type found on this platform!" )
endif()

if( SIZEOF_UINT32 EQUAL 4 )
    set( USIZE32 "uint32_t" )
elseif( SIZEOF_SHORT EQUAL 4 )
    set( USIZE32 "unsigned short" )
elseif( SIZEOF_INT EQUAL 4 )
    set( USIZE32 "unsigned int" )
elseif( SIZEOF_LONG EQUAL 4 )
    set( USIZE32 "unsigned long" )
elseif( SIZEOF_U_INT EQUAL 4 )
    set( USIZE32 "u_int32_t" )
else()
    message( FATAL_ERROR "No unsigned 32 bit type found on this platform!" )
endif()

if( SIZEOF_INT64 EQUAL 8 )
    set( SIZE64 "int64_t" )
elseif( SIZEOF_INT EQUAL 8 )
    set( SIZE64 "int" )
elseif( SIZEOF_LONG EQUAL 8 )
    set( SIZE64 "long" )
elseif( SIZEOF_LONG_LONG EQUAL 8 )
    set( SIZE64 "long long" )
else()
    message( FATAL_ERROR "No 64 bit type found on this platform!" )
endif()

set( INCLUDE_INTTYPES_H ${HAVE_INTTYPES_H} )
set( INCLUDE_STDINT_H ${HAVE_STDINT_H} )
set( INCLUDE_SYS_TYPES_H ${HAVE_SYS_TYPES_H} )

configure_file( config_types.h.in public/ogg/config_types.h )

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )

