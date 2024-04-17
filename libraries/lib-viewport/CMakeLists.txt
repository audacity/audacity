#[[
Viewport defines a callback facade for horizontal and vertical scrollbars, and
provides methods to scroll and zoom the view of tracks.
]]

set( SOURCES
   Viewport.cpp
   Viewport.h
)
set( LIBRARIES
   lib-playable-track-interface
   lib-snapping-interface
   lib-track-selection-interface
)
audacity_library( lib-viewport "${SOURCES}" "${LIBRARIES}"
   "" ""
)
