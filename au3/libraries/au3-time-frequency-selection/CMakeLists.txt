#[[
Per-project state of selected time interval, frequency interval of simple
spectral selection, and play region
]]

set( TARGET au3-time-frequency-selection )

set( SOURCES
   ProjectSelectionManager.cpp
   ProjectSelectionManager.h
   SelectedRegion.cpp
   SelectedRegion.h
   ViewInfo.cpp
   ViewInfo.h
)
set( LIBRARIES
PUBLIC
   au3-components
   au3-preferences
   au3-registries
   au3-screen-geometry
   au3-utility
   au3-xml
PRIVATE
   au3-basic-ui
   au3-numeric-formats
   au3-project
   au3-project-history
   au3-project-rate
   au3-snapping
   au3-strings
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
