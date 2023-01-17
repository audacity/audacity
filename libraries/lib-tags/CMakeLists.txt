#[[
A structure containing audio file metadata (title, artist, etc.)
]]

set( SOURCES
   Tags.cpp
   Tags.h
)
set( LIBRARIES
   lib-project-history-interface
)
audacity_library( lib-tags "${SOURCES}" "${LIBRARIES}"
   "" "" )
