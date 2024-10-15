#[[
The registry of pages in the Preferences dialog and some of those pages
]]

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
