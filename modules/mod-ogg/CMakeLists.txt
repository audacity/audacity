set( TARGET mod-ogg )

set( SOURCES
      ImportOGG.cpp
      ExportOGG.cpp
      OGG.cpp
)

set( LIBRARIES
   PRIVATE
      lib-import-export-interface
      Ogg::ogg
      Vorbis::vorbis
      Vorbis::vorbisfile
      Vorbis::vorbisenc
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
