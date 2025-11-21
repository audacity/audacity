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
   lib-shuttlegui-interface
)
audacity_library( lib-export-ui "${SOURCES}" "${LIBRARIES}"
   "" ""
)
