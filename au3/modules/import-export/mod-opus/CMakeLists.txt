#[[
Opus import/export.
]]

set( TARGET au3-mod-opus )

set( SOURCES
   ExportOpus.cpp
   ExportOpus.h
   ImportOpus.cpp
   ImportOpus.h
)

set( LIBRARIES
PUBLIC
   Ogg::ogg
   Opus::opus
   opusfile::opusfile
   au3-import-export
   au3-mixer
   au3-string-utils
   au3-tags
   au3-track
   au3-utility
   au3-wave-track
PRIVATE
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
