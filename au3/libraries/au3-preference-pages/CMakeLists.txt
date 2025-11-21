#[[
The registry of pages in the Preferences dialog and some of those pages
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   LibraryPrefs.cpp
   LibraryPrefs.h
   PrefsPanel.cpp
   PrefsPanel.h
)
set( LIBRARIES
   lib-registries-interface
   lib-shuttlegui-interface
   lib-wx-wrappers-interface
)
audacity_library( lib-preference-pages "${SOURCES}" "${LIBRARIES}"
   "" ""
)
