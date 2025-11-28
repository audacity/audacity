#[[
A structure containing audio file metadata (title, artist, etc.)
]]

set( SOURCES
   Tags.cpp
   Tags.h
)
set( LIBRARIES
   au3-project-history-interface
   au3-file-formats-interface
)
audacity_library( au3-tags "${SOURCES}" "${LIBRARIES}"
   "" "" )
