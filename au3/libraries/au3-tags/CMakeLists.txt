#[[
A structure containing audio file metadata (title, artist, etc.)
]]

set( TARGET au3-tags )

set( SOURCES
   Tags.cpp
   Tags.h
)
set( LIBRARIES
   au3-project-history-interface
   au3-file-formats-interface
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" "" )
