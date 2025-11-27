#[[
Per-project state of selected time interval, frequency interval of simple
spectral selection, and play region
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

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
)
audacity_library( au3-time-frequency-selection "${SOURCES}" "${LIBRARIES}"
   "" ""
)
