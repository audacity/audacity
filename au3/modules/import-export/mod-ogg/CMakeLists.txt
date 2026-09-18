#[[
Ogg Vorbis import/export.
]]

set( TARGET au3-mod-ogg )

set( SOURCES
   ExportOGG.cpp
   ExportOGG.h
   ImportOGG.cpp
   ImportOGG.h
)

set( LIBRARIES
PUBLIC
   Ogg::ogg
   Vorbis::vorbis # bundles vorbis, vorbisenc and vorbisfile
   au3-files
   au3-import-export
   au3-mixer
   au3-tags
   au3-track
   au3-wave-track
PRIVATE
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
