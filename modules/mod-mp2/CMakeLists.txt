set( TARGET mod-mp2 )

set( SOURCES
      ExportMP2.cpp
      MP2.cpp
)

set( LIBRARIES
   PRIVATE
      lib-import-export-interface
      twolame
)

if ( USE_LIBID3TAG )
      list ( APPEND LIBRARIES PRIVATE libid3tag::libid3tag)
endif()

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
