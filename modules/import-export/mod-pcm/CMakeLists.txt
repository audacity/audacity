#[[
PCM (that is .wav file) import and export
]]

set( TARGET mod-pcm )

set( SOURCES
      ImportPCM.cpp
      ExportPCM.cpp
      PCM.cpp
)

if ( USE_LIBID3TAG )
      list ( APPEND LIBRARIES PRIVATE libid3tag::libid3tag)
endif()

set (EXTRA_CLUSTER_NODES "${LIBRARIES}" PARENT_SCOPE)

list (APPEND LIBRARIES
   PRIVATE
      lib-import-export-interface
      lib-file-formats-interface
)


audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
