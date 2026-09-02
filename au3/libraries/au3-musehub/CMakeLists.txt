#[[
A library, that allows to interface with musehub
]]

set( TARGET au3-musehub )

set( SOURCES
   MuseHubService.cpp
   MuseHubService.h
)
set( LIBRARIES
PRIVATE
   au3-network-manager
   au3-string-utils
   au3-strings # Languages
   rapidjson::rapidjson
   wxBase # au3-strings/Languages.h pulls in wx/string.h
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
