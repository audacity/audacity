#[[
MP2 export
]]

set( TARGET mod-mp2 )

set( SOURCES
      ExportMP2.cpp
      MP2.cpp
)

set( LIBRARIES
   PRIVATE
      twolame
)

if ( USE_LIBID3TAG )
      list ( APPEND LIBRARIES PRIVATE libid3tag::libid3tag)
endif()

set (EXTRA_CLUSTER_NODES "${LIBRARIES}" PARENT_SCOPE)

list (APPEND LIBRARIES
   lib-import-export-interface
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
