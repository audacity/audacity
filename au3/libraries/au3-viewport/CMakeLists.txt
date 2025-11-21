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
   lib-playable-track-interface
   lib-snapping-interface
   lib-track-selection-interface
)
audacity_library( lib-viewport "${SOURCES}" "${LIBRARIES}"
   "" ""
)
