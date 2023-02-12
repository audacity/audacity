#[[
A small language for defining formats for reporting time and frequency
quantities with various units.

Definitions of may commonly used formats.
]]

set( SOURCES
   NumericConverter.cpp
   NumericConverter.h
)
set( LIBRARIES
   lib-components-interface
   lib-math-interface
)
audacity_library( lib-numeric-formats "${SOURCES}" "${LIBRARIES}"
   "" ""
)

