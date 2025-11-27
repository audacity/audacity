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
   au3-registries-interface
   au3-shuttlegui-interface
   au3-wx-wrappers-interface
)
audacity_library( au3-preference-pages "${SOURCES}" "${LIBRARIES}"
   "" ""
)
