#[[
A small language for defining formats for reporting time and frequency
quantities with various units.

Definitions of may commonly used formats.

Per-project choices of numeric formats.
]]

set( SOURCES
   NumericConverter.cpp
   NumericConverter.h
   ProjectNumericFormats.cpp
   ProjectNumericFormats.h
)
set( LIBRARIES
   lib-math-interface
   lib-project-interface
)
audacity_library( lib-numeric-formats "${SOURCES}" "${LIBRARIES}"
   "" ""
)

