
add_library( ${TARGET} STATIC )

def_vars()

list( APPEND SOURCES
   PRIVATE

      # sqlite

      ${TARGET_ROOT}/sqlite3.c
      ${TARGET_ROOT}/sqlite3.h
)

list( APPEND INCLUDES
   PUBLIC
      ${TARGET_ROOT}
)

list( APPEND DEFINES
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

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )

