#[[
PCM (that is .wav file) import and export
]]

set( TARGET mod-pcm )

set( SOURCES
      CueTrackAttachment.cpp
      CueTrackAttachment.h
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
      au3-label-track
)


audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
