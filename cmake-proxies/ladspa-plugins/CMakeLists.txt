
set( TARGET sc4_1882 )
set( TARGET_ROOT "${_SRCDIR}" )

message( STATUS "========== Configuring ${TARGET} ==========" )

add_library( ${TARGET} MODULE EXCLUDE_FROM_ALL )

def_vars()

list( APPEND SOURCES
   PRIVATE
      ${_SRCDIR}/sc4_1882.c
      ${_SRCDIR}/util/db.c
      ${_SRCDIR}/util/rms.c
)

list( APPEND INCLUDES
   PRIVATE
      ${_SRCDIR}
      ${topdir}/src/effects/ladspa
)

list( APPEND OPTIONS
   PRIVATE
      $<$<C_COMPILER_ID:AppleClang,Clang,GNU>:-D_init=__attribute__\(\(constructor\)\)_${TARGET}_init>
)

set_target_property_all( ${TARGET}
   LIBRARY_OUTPUT_DIRECTORY
      "${_DEST}/plug-ins"
)

set_target_properties( ${TARGET}
   PROPERTIES
      PREFIX ""
      FOLDER "lib-src"
)

target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )

