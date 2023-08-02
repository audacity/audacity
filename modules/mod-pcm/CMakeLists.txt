set( TARGET mod-pcm )

set( SOURCES
      ImportPCM.cpp
      ExportPCM.cpp
      PCM.cpp
)

set( LIBRARIES
   PRIVATE
      lib-import-export-interface
      lib-file-formats-interface
      lib-wx-init-interface
)

if ( USE_LIBID3TAG )
      list ( APPEND LIBRARIES PRIVATE libid3tag::libid3tag)
endif()

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
