#[[
WavPack import/export.
]]

set( TARGET au3-mod-wavpack )

set( SOURCES
   ExportWavPack.cpp
   ExportWavPack.h
   ImportWavPack.cpp
   ImportWavPack.h
)

set( LIBRARIES
PUBLIC
   wavpack::wavpack
   au3-import-export
   au3-math
   au3-mixer
PRIVATE
   au3-files
   au3-string-utils
   au3-tags
   au3-track
   au3-wave-track
   rapidjson::rapidjson
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
