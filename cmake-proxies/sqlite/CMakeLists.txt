
add_library( ${TARGET} SHARED )

def_vars()

set( SOURCES
   PRIVATE

      # sqlite

      ${TARGET_ROOT}/sqlite3.c
      ${TARGET_ROOT}/sqlite3.h
)

set( INCLUDES
   PUBLIC
      ${TARGET_ROOT}
)

set( DEFINES

   PRIVATE
      #
      # Recommended in SQLite docs
      #
      SQLITE_DQS=0
      SQLITE_DEFAULT_MEMSTATUS=0
      SQLITE_DEFAULT_SYNCHRONOUS=1
      SQLITE_DEFAULT_WAL_SYNCHRONOUS=1
      SQLITE_LIKE_DOESNT_MATCH_BLOBS
      SQLITE_MAX_EXPR_DEPTH=0
      SQLITE_OMIT_DEPRECATED
      SQLITE_OMIT_SHARED_CACHE
      SQLITE_USE_ALLOCA
      SQLITE_OMIT_AUTOINIT
      $<$<BOOL:${HAVE_FDATASYNC}>:HAVE_FDATASYNC>
      $<$<BOOL:${HAVE_GMTIME_R}>:HAVE_GMTIME_R>
      $<$<BOOL:${HAVE_ISNAN}>:HAVE_ISNAN>
      $<$<BOOL:${HAVE_LOCALTIME_R}>:HAVE_LOCALTIME_R>
      $<$<BOOL:${HAVE_LOCALTIME_S}>:HAVE_LOCALTIME_S>
)

if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   set(DEFINES ${DEFINES}
      INTERFACE
         "SQLITE_API=__declspec(dllimport)"
      PRIVATE
         "SQLITE_API=__declspec(dllexport)"
   )
else()
   set(DEFINES ${DEFINES}
      INTERFACE
         "SQLITE_API=__attribute__((visibility(\"default\")))"
   )
endif()

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )

set_target_properties(${TARGET} PROPERTIES POSITION_INDEPENDENT_CODE On)

if (CMAKE_SYSTEM_NAME MATCHES "Windows")
   set( DIRECTORY_PROPERTY RUNTIME_OUTPUT_DIRECTORY )
else ()
   set( DIRECTORY_PROPERTY LIBRARY_OUTPUT_DIRECTORY )
endif ()

set_target_property_all( ${TARGET} ${DIRECTORY_PROPERTY} "${_DESTDIR}/${_PKGLIB}" )

if( NOT CMAKE_SYSTEM_NAME MATCHES "Windows|Darwin" )
   set_target_property_all(${TARGET} INSTALL_RPATH "$ORIGIN")
   set_target_property_all(${TARGET} BUILD_RPATH "$ORIGIN")
   install(TARGETS ${TARGET} DESTINATION ${_PKGLIB} )
endif()

