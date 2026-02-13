#[[
Dynamic-range processor and utilities
]]

set( TARGET au3-dynamic-range-processor )

set( SOURCES
   CompressorProcessor.cpp
   CompressorProcessor.h
   DownwardMeterValueProvider.cpp
   DownwardMeterValueProvider.h
   DynamicRangeProcessorClock.cpp
   DynamicRangeProcessorClock.h
   DynamicRangeProcessorDummyOutputs.h
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
   au3-utility
   au3-strings
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
