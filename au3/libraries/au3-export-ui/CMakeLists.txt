#[[
UI extension for exporting classes.
]]

set( TARGET au3-export-ui )

set( SOURCES
   ExportOptionsUIServices.cpp
   ExportOptionsUIServices.h
)
set( LIBRARIES
   au3-shuttlegui-interface
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
