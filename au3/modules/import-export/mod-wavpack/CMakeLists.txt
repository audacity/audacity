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
   au3-import-export
PRIVATE
   au3-files
   au3-math
   au3-mixer
   au3-string-utils
   au3-tags
   au3-track
   au3-wave-track
   rapidjson::rapidjson
   wavpack::wavpack
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
