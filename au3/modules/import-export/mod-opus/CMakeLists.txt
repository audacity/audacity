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
   au3-import-export
PRIVATE
   au3-mixer
   au3-string-utils
   au3-tags
   au3-track
   au3-utility
   au3-wave-track
   Ogg::ogg
   Opus::opus
   opusfile::opusfile
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
