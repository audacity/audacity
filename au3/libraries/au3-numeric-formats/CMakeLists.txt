#[[
A small language for defining formats for reporting time and frequency
quantities with various units.

Definitions of may commonly used formats.

Per-project choices of numeric formats.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   Beats.cpp
   Beats.h
   NumericConverter.cpp
   NumericConverter.h
   NumericConverterFormats.cpp
   NumericConverterFormats.h
   NumericConverterFormatter.cpp
   NumericConverterFormatter.h
   NumericConverterFormatterContext.cpp
   NumericConverterFormatterContext.h
   NumericConverterRegistry.cpp
   NumericConverterRegistry.h
   NumericConverterType.cpp
   NumericConverterType.h
   ProjectNumericFormats.cpp
   ProjectNumericFormats.h
   ProjectTimeSignature.cpp
   ProjectTimeSignature.h

   formatters/BeatsNumericConverterFormatter.cpp
   formatters/BeatsNumericConverterFormatter.h
   formatters/ParsedNumericConverterFormatter.cpp
   formatters/ParsedNumericConverterFormatter.h
)
set( LIBRARIES
   au3-math-interface
   au3-project-rate-interface
)
audacity_library( au3-numeric-formats "${SOURCES}" "${LIBRARIES}"
   "" ""
)

