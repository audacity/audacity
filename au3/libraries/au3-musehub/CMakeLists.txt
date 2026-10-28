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
   au3-network-manager-interface
   au3-strings-interface # Languages
   PRIVATE
   rapidjson::rapidjson
   Qt::Core
   Qt::Gui
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
