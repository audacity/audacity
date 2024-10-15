#[[
MPG123 import
]]

set( TARGET mod-mpg123 )

set( SOURCES
      ImportMP3_MPG123.cpp
      MPG123.cpp
)

set( LIBRARIES
   PRIVATE
      mpg123::libmpg123
)

if ( USE_LIBID3TAG )
      list ( APPEND LIBRARIES PRIVATE libid3tag::libid3tag)
endif()

set (EXTRA_CLUSTER_NODES "${LIBRARIES}" PARENT_SCOPE)

list(APPEND LIBRARIES
   lib-import-export-interface
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
