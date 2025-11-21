#[[
Dynamic-range processor and utilities
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   CompressorProcessor.cpp
   CompressorProcessor.h
   DownwardMeterValueProvider.cpp
   DownwardMeterValueProvider.h
   DynamicRangeProcessorClock.cpp
   DynamicRangeProcessorClock.h
   DynamicRangeProcessorHistory.cpp
   DynamicRangeProcessorHistory.h
   DynamicRangeProcessorTypes.h
   DynamicRangeProcessorUtils.cpp
   DynamicRangeProcessorUtils.h
   MeterValueProvider.cpp
   MeterValueProvider.h
   UpwardMeterValueProvider.cpp
   UpwardMeterValueProvider.h
   SimpleCompressor/GainReductionComputer.cpp
   SimpleCompressor/GainReductionComputer.h
   SimpleCompressor/LookAheadGainReduction.cpp
   SimpleCompressor/LookAheadGainReduction.h
)
set( LIBRARIES
   lib-utility
   lib-strings
)
audacity_library( lib-dynamic-range-processor "${SOURCES}" "${LIBRARIES}"
   "" ""
)
