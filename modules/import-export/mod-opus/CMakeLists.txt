set( TARGET mod-opus )

set( SOURCES
      ImportOpus.cpp
      ExportOpus.cpp
      Opus.cpp
)

set( LIBRARIES
   PRIVATE
      lib-import-export-interface
      Opus::opus
      opusfile::opusfile
      Ogg::ogg
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
