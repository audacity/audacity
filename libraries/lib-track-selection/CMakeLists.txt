#[[
Utilities for changing the binary selected state of tracks in a project, which
includes some state to record the last-clicked track, to implement shift-click
of a range.

Also sync-locked state of the project, and the criteria for sync lock grouping
of tracks.
]]

set( SOURCES
   SelectionState.cpp
   SelectionState.h
   SyncLock.cpp
   SyncLock.h
)
set( LIBRARIES
   lib-time-frequency-selection-interface
)
audacity_library( lib-track-selection "${SOURCES}" "${LIBRARIES}"
   "" ""
)
