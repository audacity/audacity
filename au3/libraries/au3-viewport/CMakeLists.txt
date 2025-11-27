#[[
Viewport defines a callback facade for horizontal and vertical scrollbars, and
provides methods to scroll and zoom the view of tracks.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   Viewport.cpp
   Viewport.h
)
set( LIBRARIES
   au3-playable-track-interface
   au3-snapping-interface
   au3-track-selection-interface
)
audacity_library( au3-viewport "${SOURCES}" "${LIBRARIES}"
   "" ""
)
