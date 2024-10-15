#[[
OGG import and export
]]

set( TARGET mod-ogg )

set( SOURCES
      ImportOGG.cpp
      ExportOGG.cpp
      OGG.cpp
)

set( LIBRARIES
   PRIVATE
      Ogg::ogg
      Vorbis::vorbis
      Vorbis::vorbisfile
      Vorbis::vorbisenc
)

set (EXTRA_CLUSTER_NODES "${LIBRARIES}" PARENT_SCOPE)

list(APPEND LIBRARIES
   lib-import-export-interface
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
