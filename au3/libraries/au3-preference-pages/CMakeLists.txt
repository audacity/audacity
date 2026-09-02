#[[
The registry of pages in the Preferences dialog and some of those pages
]]

set( TARGET au3-preference-pages )

set( SOURCES
   LibraryPrefs.cpp
   LibraryPrefs.h
   PrefsPanel.cpp
   PrefsPanel.h
)
set( LIBRARIES
PUBLIC
   au3-components
   au3-registries
PRIVATE
   au3-shuttlegui
   au3-strings
   au3-wx-wrappers
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
