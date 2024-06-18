#[[
Dynamic-range processor and utilities
]]

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
)
audacity_library( lib-dynamic-range-processor "${SOURCES}" "${LIBRARIES}"
   "" ""
)
