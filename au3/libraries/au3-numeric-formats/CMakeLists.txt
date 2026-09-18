#[[
A small language for defining formats for reporting time and frequency
quantities with various units.

Definitions of may commonly used formats.

Per-project choices of numeric formats.
]]

set( TARGET au3-numeric-formats )

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
PUBLIC
   au3-components
   au3-preferences
   au3-registries
   au3-strings
   au3-utility
PRIVATE
   au3-math
   au3-project
   au3-project-rate
   au3-xml
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)

