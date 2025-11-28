#[[
UI extension for exporting classes.
]]

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
