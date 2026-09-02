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
PUBLIC
   au3-registries
   au3-utility
PRIVATE
   au3-basic-ui
   au3-playable-track
   au3-project
   au3-project-history
   au3-snapping
   au3-strings
   au3-time-frequency-selection
   au3-track
   au3-track-selection
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
