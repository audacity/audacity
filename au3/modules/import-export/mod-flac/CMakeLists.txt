#[[
FLAC import/export.
]]

set( TARGET au3-mod-flac )

set( SOURCES
   ExportFLAC.cpp
   ExportFLAC.h
   ImportFLAC.cpp
   ImportFLAC.h
)

set( LIBRARIES
PUBLIC
   FLAC::FLAC
   FLAC::FLAC++
   au3-import-export
   au3-mixer
   au3-tags
   au3-track
   au3-wave-track
PRIVATE
   au3-math
   rapidjson::rapidjson
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
