#[[
FLAC import and export
]]

set( TARGET mod-flac )

set( SOURCES
      ImportFLAC.cpp
      ExportFLAC.cpp
      FLAC.cpp
)

set( LIBRARIES
   PRIVATE
      FLAC::FLAC
      FLAC::FLAC++
)

if ( USE_LIBID3TAG )
      list ( APPEND LIBRARIES PRIVATE libid3tag::libid3tag)
endif()

set (EXTRA_CLUSTER_NODES "${LIBRARIES}" PARENT_SCOPE)

list(APPEND LIBRARIES
   lib-import-export-interface
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
