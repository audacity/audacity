#[[
Viewport defines a callback facade for horizontal and vertical scrollbars, and
provides methods to scroll and zoom the view of tracks.
]]

set( TARGET au3-viewport )

set( SOURCES
   Viewport.cpp
   Viewport.h
)
set( LIBRARIES
   au3-playable-track-interface
   au3-snapping-interface
   au3-track-selection-interface
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
