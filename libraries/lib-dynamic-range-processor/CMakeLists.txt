#[[
Dynamic-range processor and utilities
]]

set( SOURCES
   CompressorProcessor.cpp
   CompressorProcessor.h
   DynamicRangeProcessorHistory.cpp
   DynamicRangeProcessorHistory.h
   DynamicRangeProcessorTypes.h
   SimpleCompressor/GainReductionComputer.cpp
   SimpleCompressor/GainReductionComputer.h
   SimpleCompressor/LookAheadGainReduction.cpp
   SimpleCompressor/LookAheadGainReduction.h
)
set( LIBRARIES
   lib-utility
)
audacity_library( lib-dynamic-range-processor "${SOURCES}" "${LIBRARIES}"
   "" ""
)
