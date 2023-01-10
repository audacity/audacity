set( TARGET mod-mpg123 )

set( SOURCES
      ImportMP3_MPG123.cpp
      MPG123.cpp
)

set( LIBRARIES
   PRIVATE
      lib-import-export-interface
      mpg123::libmpg123
)

if ( USE_LIBID3TAG )
      list ( APPEND LIBRARIES PRIVATE libid3tag::libid3tag)
endif()

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
