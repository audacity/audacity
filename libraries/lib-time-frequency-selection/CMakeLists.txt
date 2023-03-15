#[[
Per-project state of selected time interval, frequency interval of simple
spectral selection, and play region
]]

set( SOURCES
   SelectedRegion.cpp
   SelectedRegion.h
   ViewInfo.cpp
   ViewInfo.h
)
set( LIBRARIES
   lib-project-history-interface
   lib-screen-geometry-interface
)
audacity_library( lib-time-frequency-selection "${SOURCES}" "${LIBRARIES}"
   "" ""
)
