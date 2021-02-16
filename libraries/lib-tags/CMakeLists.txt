#[[
A structure containing audio file metadata (title, artist, etc.)
]]

set( SOURCES
   Tags.cpp
   Tags.h
)
set( LIBRARIES
   lib-project-history-interface
   lib-registries-interface
   lib-xml-interface
   PRIVATE
   wxBase
)
audacity_library( lib-tags "${SOURCES}" "${LIBRARIES}"
   "" "" )
