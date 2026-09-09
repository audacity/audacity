#[[
MP3 import via mpg123.
]]

set( TARGET au3-mod-mpg123 )

set( SOURCES
   ImportMP3_MPG123.cpp
   ImportMP3_MPG123.h
)

set( LIBRARIES
PUBLIC
   mpg123::libmpg123
   au3-import-export
   au3-wave-track
PRIVATE
   au3-project
   au3-string-utils
   au3-tags
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
