set( TARGET mod-cl )

set( SOURCES
      ExportCL.cpp
      CL.cpp
)

set( LIBRARIES
      Audacity
)

if ( USE_LIBID3TAG )
      list ( APPEND LIBRARIES PUBLIC libid3tag::libid3tag)
endif()

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
