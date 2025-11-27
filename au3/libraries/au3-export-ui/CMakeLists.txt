#[[
UI extension for exporting classes.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   ExportOptionsUIServices.cpp
   ExportOptionsUIServices.h
)
set( LIBRARIES
   au3-shuttlegui-interface
)
audacity_library( au3-export-ui "${SOURCES}" "${LIBRARIES}"
   "" ""
)
