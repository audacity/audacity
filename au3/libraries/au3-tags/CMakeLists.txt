#[[
A structure containing audio file metadata (title, artist, etc.)
]]

set( TARGET au3-tags )

set( SOURCES
   Tags.cpp
   Tags.h
)
set( LIBRARIES
PUBLIC
   au3-project-history
   au3-registries
   au3-strings
   au3-xml
PRIVATE
   au3-file-formats
   au3-files
   au3-preferences
   au3-project
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" "" )
