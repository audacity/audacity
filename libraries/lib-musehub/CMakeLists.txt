#[[
A library, that allows to interface with musehub
]]

set( SOURCES
   MuseHubService.cpp
   MuseHubService.h
)
set( LIBRARIES
   lib-network-manager-interface
   lib-strings-interface # Languages
   rapidjson::rapidjson
)

audacity_library( lib-musehub "${SOURCES}" "${LIBRARIES}"
   "" ""
)
