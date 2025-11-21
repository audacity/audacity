#[[
UI extension for exporting classes.
]]

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
