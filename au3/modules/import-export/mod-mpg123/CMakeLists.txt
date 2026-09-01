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
   au3-import-export
PRIVATE
   au3-project
   au3-string-utils
   au3-tags
   au3-wave-track
   mpg123::libmpg123
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
