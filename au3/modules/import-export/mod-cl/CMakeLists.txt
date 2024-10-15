#[[
Export of files with a command passed to a command-line shell
]]

set( TARGET mod-cl )

set( SOURCES
      ExportCL.cpp
      CL.cpp
)

set( LIBRARIES
   PRIVATE
      Audacity
)

if ( USE_LIBID3TAG )
      list ( APPEND LIBRARIES PRIVATE libid3tag::libid3tag)
endif()

set (EXTRA_CLUSTER_NODES "${LIBRARIES}" PARENT_SCOPE)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
