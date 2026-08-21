#[[
A library, that allows to interface with musehub
]]

set( TARGET au3-musehub )

set( SOURCES
   MuseHubService.cpp
   MuseHubService.h
)
set( LIBRARIES
   au3-network-manager-interface
   au3-strings-interface # Languages
   PRIVATE
   rapidjson::rapidjson
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
