#[[
A library, that allows to interface with musehub
]]

set( TARGET au3-musehub )

set( SOURCES
   MuseHubService.cpp
   MuseHubService.h
   MuseHubUrlHandler.cpp
   MuseHubUrlHandler.h
)
set( LIBRARIES
PRIVATE
   au3-network-manager
   au3-string-utils
   au3-strings # Languages
   rapidjson::rapidjson
   Qt::Core
   Qt::Gui
   wxBase # au3-strings/Languages.h pulls in wx/string.h
)

if(APPLE)
   list(APPEND SOURCES platform/macos/MuseHubRegistration.mm)
   list(APPEND LIBRARIES PRIVATE "-framework AppKit")
elseif(WIN32)
   list(APPEND SOURCES platform/windows/MuseHubRegistration.cpp)
   list(APPEND LIBRARIES PRIVATE shlwapi)
else()
   list(APPEND SOURCES platform/stub/MuseHubRegistration.cpp)
endif()

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
