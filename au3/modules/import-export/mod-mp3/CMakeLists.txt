#[[
MP3 export via LAME.
]]

set( TARGET au3-mod-mp3 )

set( SOURCES
   ExportMP3.cpp
   ExportMP3.h
)

set( LIBRARIES
PUBLIC
   libmp3lame::libmp3lame
   au3-files
   au3-import-export
   au3-mixer
   au3-strings
PRIVATE
   au3-basic-ui
   au3-math
   au3-preferences
   au3-project
   au3-tags
   au3-track
   au3-utility
   rapidjson::rapidjson
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
