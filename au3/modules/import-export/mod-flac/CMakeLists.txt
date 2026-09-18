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
   au3-import-export
PRIVATE
   au3-math
   au3-mixer
   au3-tags
   au3-track
   au3-wave-track
   FLAC::FLAC
   FLAC::FLAC++
   rapidjson::rapidjson
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
