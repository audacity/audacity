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
   au3-snapping-interface
   au3-strings-interface
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
