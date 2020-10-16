#[[
Adds user interface for time tracks, including menu items
]]

set( SOURCES
   TimeTrackControls.cpp
   TimeTrackControls.h
   TimeTrackMenuItems.cpp
   TimeTrackVRulerControls.cpp
   TimeTrackVRulerControls.h
   TimeTrackVZoomHandle.cpp
   TimeTrackVZoomHandle.h
   TimeTrackView.cpp
   TimeTrackView.h
)
set( LIBRARIES
   Audacity
)
audacity_module( mod-time-track-ui "${SOURCES}" "${LIBRARIES}"
   "" "" )
