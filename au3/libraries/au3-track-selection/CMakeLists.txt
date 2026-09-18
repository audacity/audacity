#[[
Utilities for changing the binary selected state of tracks in a project, which
includes some state to record the last-clicked track, to implement shift-click
of a range.

Also sync-locked state of the project, and the criteria for sync lock grouping
of tracks.

Also the logic to set and get the unique focused track of the project, with a
callback interface for screen reader notifications.
]]

set( TARGET au3-track-selection )

set( SOURCES
   TrackFocus.cpp
   TrackFocus.h
)
set( LIBRARIES
PUBLIC
   au3-registries
   au3-utility
PRIVATE
   au3-basic-ui
   au3-project
   au3-strings
   au3-time-frequency-selection
   au3-track
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
