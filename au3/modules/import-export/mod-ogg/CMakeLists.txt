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
   au3-import-export
PRIVATE
   au3-files
   au3-mixer
   au3-tags
   au3-track
   au3-wave-track
   Ogg::ogg
   Vorbis::vorbis
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
