#[[
Per-project state of selected time interval, frequency interval of simple
spectral selection, and play region
]]

set( SOURCES
   ProjectSelectionManager.cpp
   ProjectSelectionManager.h
   SelectedRegion.cpp
   SelectedRegion.h
   ViewInfo.cpp
   ViewInfo.h
)
set( LIBRARIES
   lib-snapping-interface
)
audacity_library( lib-time-frequency-selection "${SOURCES}" "${LIBRARIES}"
   "" ""
)
