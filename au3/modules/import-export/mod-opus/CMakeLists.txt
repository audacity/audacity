#[[
Opus import and export
]]

set( TARGET mod-opus )

set( SOURCES
      ImportOpus.cpp
      ExportOpus.cpp
      Opus.cpp
)

set( LIBRARIES
   PRIVATE
      Opus::opus
      opusfile::opusfile
      Ogg::ogg
)

set (EXTRA_CLUSTER_NODES "${LIBRARIES}" PARENT_SCOPE)

list(APPEND LIBRARIES
   lib-import-export-interface
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
